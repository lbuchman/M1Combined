import { useRef, useState, useEffect, version as reactVersion } from 'react';

const COMMANDS = [
  { key: 'ict',         label: 'ICT'       },
  { key: 'progmac',    label: 'MAC PROGRAM'  },
  { key: 'flash',      label: 'FLASH'     },
  { key: 'functest',   label: 'FUNC TEST' },
  { key: 'eeprom',     label: 'EEPROM'    },
  { key: 'pingM1apps', label: 'APP'       },
  { key: 'makelabel',  label: 'PRINT LABEL' }
];

const PROGRESS = { ict:5, progmac:3, flash:40, functest:43, eeprom:5, pingM1apps:3, makelabel:8 };
const RETEST_SKIP = new Set(['flash']);
const API = localStorage.getItem('m1-api') || `http://${window.location.hostname}:3300`;
// TODO: Read max idle timeout from restServer config. Default is 1.5h for now.
const MAX_IDLE_MS = 90 * 60 * 1000;
const PROD_PIN_BYPASS = '1234';
const DEBUG_PIN_BYPASS = '4321';
const DEBUG_PIN_BYPASS_ALT = '1234';
const MAX_LOG_LINES = 500;

function initLeds() {
  return Object.fromEntries(COMMANDS.map(c => [c.key, 'idle']));
}

export default function App() {
  const [serial,   setSerial]   = useState('');
  const [debug,    setDebug]    = useState('1');
  const [appMode,  setAppMode]  = useState('locked');
  const [busy,     setBusy]     = useState(false);
  const [leds,     setLeds]     = useState(initLeds);
  const [progress, setProgress] = useState(0);
  const [result,    setResult]    = useState(null);   // { ok, step, description }
  const [powerState, setPowerState] = useState('auto'); // 'auto', 'on', 'off'
  const [poeState,   setPoeState]   = useState('auto'); // 'auto', 'on', 'off'
  const [machineName, setMachineName] = useState('FC?');
  const [snapVersion, setSnapVersion] = useState('unknown');
  const [fwVersion, setFwVersion] = useState('unknown');
  const [versionModal, setVersionModal] = useState(false);
  const [pinModal,   setPinModal]   = useState(false);
  const [pinEntry,   setPinEntry]   = useState('');
  const [pinError,   setPinError]   = useState(false);
  const [pinTargetMode, setPinTargetMode] = useState('debug');
  const [changePinModal, setChangePinModal] = useState(false);
  const [changePinMode, setChangePinMode] = useState('debug');
  const [newPin,     setNewPin]     = useState('');
  const [newPinStep, setNewPinStep] = useState(1); // 1=enter new, 2=confirm
  const [newPinFirst, setNewPinFirst] = useState('');
  const [fakeServer, setFakeServer] = useState(false);
  const [prodReauthRequired, setProdReauthRequired] = useState(false);
  const [logLines, setLogLines] = useState([]);
  const [logPaused, setLogPaused] = useState(false);
  const [logConnected, setLogConnected] = useState(false);
  const [logError, setLogError] = useState('');
  const stopRef = useRef(false);
  const lastActivityRef = useRef(Date.now());
  const logViewportRef = useRef(null);
  const logSourceRef = useRef(null);
  const isDebug = appMode === 'debug';
  const isLocked = appMode === 'locked';

  function getFakePin(mode) {
    const key = `mnplus-fake-pin-${mode}`;
    return localStorage.getItem(key) || (mode === 'production' ? '1223' : '4321');
  }

  async function apiPost(path, payload) {
    try {
      const res = await fetch(`${API}${path}`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(payload)
      });
      return await res.json();
    } catch {
      setFakeServer(true);
      if (path === '/auth') {
        return { status: payload.pin === getFakePin(payload.mode) ? 'OK' : 'FAILED' };
      }
      if (path === '/changepin') {
        localStorage.setItem(`mnplus-fake-pin-${payload.mode}`, payload.pin);
        return { status: 'OK' };
      }
      if (path === '/command') {
        return { status: 'OK', ErrorDescription: 'FAKE SERVER' };
      }
      return { status: 'FAILED' };
    }
  }

  async function apiGet(path) {
    try {
      const res = await fetch(`${API}${path}`);
      return await res.json();
    } catch {
      setFakeServer(true);
      if (path === '/config') return { status: 'OK', machineName: 'FC?' };
      return { status: 'FAILED' };
    }
  }

  useEffect(() => {
    apiGet('/config').then(b => {
      if (b.machineName) setMachineName(b.machineName);
      if (b.snapVersion) setSnapVersion(String(b.snapVersion));
      if (b.fwVersion) setFwVersion(String(b.fwVersion));
    });
  }, []);

  useEffect(() => {
    const markActivity = () => { lastActivityRef.current = Date.now(); };
    const events = ['pointerdown', 'keydown', 'touchstart', 'mousemove'];
    events.forEach(e => window.addEventListener(e, markActivity, { passive: true }));

    const timer = setInterval(() => {
      if (appMode !== 'locked' && Date.now() - lastActivityRef.current >= MAX_IDLE_MS) {
        setAppMode('locked');
        setPinModal(false);
        setChangePinModal(false);
        setPinEntry('');
        setNewPin('');
        setPinError(false);
      }
    }, 1000);

    return () => {
      clearInterval(timer);
      events.forEach(e => window.removeEventListener(e, markActivity));
    };
  }, [appMode]);

  useEffect(() => {
    if (!isDebug || logPaused || !logViewportRef.current) return;
    logViewportRef.current.scrollTop = logViewportRef.current.scrollHeight;
  }, [isDebug, logPaused, logLines]);

  useEffect(() => {
    if (!isDebug) {
      if (logSourceRef.current) {
        logSourceRef.current.close();
        logSourceRef.current = null;
      }
      setLogConnected(false);
      setLogError('');
      return;
    }

    let cancelled = false;

    const pushLine = (line) => {
      if (!line || logPaused) return;
      setLogLines(prev => {
        if (prev.length > 0 && prev[prev.length - 1] === line) return prev;
        const next = [...prev, line];
        return next.length > MAX_LOG_LINES ? next.slice(next.length - MAX_LOG_LINES) : next;
      });
    };

    const loadTail = async () => {
      try {
        const res = await fetch(`${API}/logs/tail?lines=120`);
        const body = await res.json();
        if (cancelled) return;
        if (body.status === 'OK' && Array.isArray(body.lines)) {
          setLogLines(body.lines.slice(-MAX_LOG_LINES));
          setLogError('');
        } else {
          setLogError('Log tail unavailable');
        }
      } catch {
        if (!cancelled) setLogError('Log tail unavailable');
      }
    };

    loadTail();

    const source = new EventSource(`${API}/logs/stream?lines=1`);
    logSourceRef.current = source;

    source.onopen = () => {
      if (cancelled) return;
      setLogConnected(true);
      setLogError('');
    };

    source.onmessage = (evt) => {
      if (cancelled) return;
      try {
        const data = JSON.parse(evt.data);
        pushLine(data.line);
      } catch {
        // Ignore malformed event payloads.
      }
    };

    source.onerror = () => {
      if (cancelled) return;
      setLogConnected(false);
      setLogError('Reconnecting...');
    };

    return () => {
      cancelled = true;
      setLogConnected(false);
      if (logSourceRef.current) {
        logSourceRef.current.close();
        logSourceRef.current = null;
      }
    };
  }, [isDebug, logPaused]);

  // Keyboard input for PIN entry
  useEffect(() => {
    if (!pinModal && !changePinModal) return;

    const handleKeyDown = (evt) => {
      // Handle number keys 0-9
      if (evt.key >= '0' && evt.key <= '9') {
        evt.preventDefault();
        if (pinModal) pinPress(evt.key);
        else if (changePinModal) changePinPress(evt.key);
      }
      // Handle Backspace
      else if (evt.key === 'Backspace') {
        evt.preventDefault();
        if (pinModal) setPinEntry(p => p.slice(0, -1));
        else if (changePinModal) setNewPin(p => p.slice(0, -1));
      }
      // Handle Enter to confirm (optional)
      else if (evt.key === 'Enter') {
        evt.preventDefault();
        if (pinModal) {
          if (pinEntry.length >= 4) pinPress('');
        } else if (changePinModal) {
          if (newPin.length >= 4) changePinPress('');
        }
      }
    };

    window.addEventListener('keydown', handleKeyDown);
    return () => window.removeEventListener('keydown', handleKeyDown);
  }, [pinModal, changePinModal, pinEntry, newPin, newPinStep]);

  function openChangePinModal(mode) {
    setChangePinMode(mode);
    setNewPin('');
    setNewPinStep(1);
    setNewPinFirst('');
    setPinError(false);
    setChangePinModal(true);
  }

  async function changePinPress(d) {
    if (newPin.length >= 6) return;
    const next = newPin + d;
    setNewPin(next);
    if (next.length >= 4) {
      if (newPinStep === 1) {
        setNewPinFirst(next); setNewPin(''); setNewPinStep(2);
      } else {
        if (next === newPinFirst) {
          try {
            const body = await apiPost('/changepin', { pin: next, mode: changePinMode });
            if (body.status === 'OK') { setChangePinModal(false); }
            else { setPinError(true); setTimeout(() => { setNewPin(''); setNewPinStep(1); setNewPinFirst(''); setPinError(false); }, 700); }
          } catch {
            setPinError(true); setTimeout(() => { setNewPin(''); setNewPinStep(1); setNewPinFirst(''); setPinError(false); }, 700);
          }
        } else {
          setPinError(true);
          setTimeout(() => { setNewPin(''); setNewPinStep(1); setNewPinFirst(''); setPinError(false); }, 700);
        }
      }
    }
  }

  function openPinModal(mode) {
    setPinTargetMode(mode);
    setPinEntry('');
    setPinError(false);
    setPinModal(true);
  }

  function switchMode(targetMode) {
    if (targetMode === appMode) return;

    // Per operator rule: debug -> production does not need PIN unless production was invalidated.
    if (appMode === 'debug' && targetMode === 'production' && !prodReauthRequired) {
      setAppMode('production');
      setPowerState('auto');
      setPoeState('auto');
      setDebug('1');
      return;
    }

    // Moving from production to debug invalidates production until production PIN is entered again.
    if (appMode === 'production' && targetMode === 'debug') {
      setProdReauthRequired(true);
    }

    // Locked -> mode and production -> debug continue to use PIN modal.
    openPinModal(targetMode);
  }

  async function pinPress(d) {
    if (pinEntry.length >= 6) return;
    const next = pinEntry + d;
    setPinEntry(next);
    if (next.length >= 4) {
      try {
        // Temporary bypass: production/debug PINs are validated locally and skip server auth.
        const body = pinTargetMode === 'production'
          ? { status: next === PROD_PIN_BYPASS ? 'OK' : 'FAILED' }
          : pinTargetMode === 'debug'
            ? { status: (next === DEBUG_PIN_BYPASS || next === DEBUG_PIN_BYPASS_ALT) ? 'OK' : 'FAILED' }
            : await apiPost('/auth', { pin: next, mode: pinTargetMode });
        if (body.status === 'OK') {
          setPinModal(false);
          setAppMode(pinTargetMode);
          if (pinTargetMode === 'production') {
            setProdReauthRequired(false);
            setPowerState('auto');
            setPoeState('auto');
            setDebug('1');
          }
        }
        else { setPinError(true); setTimeout(() => { setPinEntry(''); setPinError(false); }, 700); }
      } catch {
        setPinError(true); setTimeout(() => { setPinEntry(''); setPinError(false); }, 700);
      }
    }
  }

  function resetPanel() {
    setLeds(initLeds());
    setProgress(0);
    setResult(null);
  }

  function setLed(key, state) {
    setLeds(prev => ({ ...prev, [key]: state }));
  }

  function validSerial() {
    return /^\d{10}$/.test(serial.trim());
  }

  function downloadLogs() {
    if (logLines.length === 0) return;
    const timestamp = new Date().toISOString().replace(/[:.]/g, '-');
    const content = `${logLines.join('\n')}\n`;
    const blob = new Blob([content], { type: 'text/plain;charset=utf-8' });
    const url = URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = url;
    link.download = `mnplus-debug-log-${timestamp}.txt`;
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
    URL.revokeObjectURL(url);
  }

  async function callCommand(key, isRetest) {
    let arg = `--serial ${serial.trim()} --debug ${debug}`;
    if (key === 'ict') arg += ` --cellBatTol used -v 2.5`;
    setLed(key, 'running');
    try {
      const body = await apiPost('/command', { command: key, argument: arg });
      const ok   = body.status === 'OK';
      setLed(key, ok ? 'ok' : 'fail');
      return { ok, description: body.ErrorDescription || '' };
    } catch (err) {
      setLed(key, 'fail');
      return { ok: false, description: err.message };
    }
  }

  async function runSequence(mode) {
    if (busy) return;
    if (!validSerial()) { setResult({ ok: false, step: '', description: 'Invalid serial number' }); return; }
    stopRef.current = false;
    setBusy(true);
    resetPanel();
    const isRetest = mode === 'retest';
    const seq = isRetest ? COMMANDS.filter(c => !RETEST_SKIP.has(c.key)) : COMMANDS;
    let done = 0;
    let failed = null;

    for (const item of seq) {
      if (stopRef.current) { failed = { step: item.label, description: 'Stopped by operator' }; break; }
      const r = await callCommand(item.key, isRetest);
      done += PROGRESS[item.key] || 5;
      setProgress(Math.min(100, done));
      if (!r.ok) { failed = { step: item.label, description: r.description }; break; }
    }

    if (!failed) {
      await apiPost('/command', { command: 'cleanup', argument: `--serial ${serial.trim()}` });
    }

    setResult(failed ? { ok: false, ...failed } : { ok: true, step: '', description: 'All tests passed' });
    setBusy(false);
  }

  function stop() { stopRef.current = true; setBusy(false); }

  async function setPower(newState) {
    setPowerState(newState);
    try {
      await apiPost('/command', { command: 'power', argument: `--state ${newState}` });
    } catch (err) {}
  }

  async function setPoe(newState) {
    setPoeState(newState);
    try {
      await apiPost('/command', { command: 'poe', argument: `--state ${newState}` });
    } catch (err) {}
  }

  async function reboot() {
    try {
      await apiPost('/command', { command: 'reboot', argument: '' });
    } catch (err) {}
  }

  return (
    <div className={`panel${!isDebug ? ' panel-prod' : ''}`}>

      {/* TITLE */}
      <div className="title-bar">
        <div className="honeywell-brand">
          <span className="honeywell-text">HONEYWELL</span>
        </div>
        <span className="title-text">MnPlus</span>
        <span className="machine-name">{machineName}</span>
        <div className="mode-wrap">
          <button className="mode-btn info-btn" onClick={() => setVersionModal(true)}>INFO</button>
          <button className={`mode-btn ${appMode === 'production' ? 'mode-on' : ''}`} onClick={() => switchMode('production')}>PRODUCTION</button>
          <button className={`mode-btn ${appMode === 'debug' ? 'mode-on' : ''}`} onClick={() => switchMode('debug')}>DEBUG</button>
        </div>
      </div>

      {/* POWER & POE SWITCHES */}
      <div className={`power-bar${!isDebug ? ' power-bar-dim' : ''}`}>
        <span className="seg-lbl">POWER</span>
        <div className="seg-group">
          <button className={`seg-btn${powerState === 'auto' ? ' seg-auto-on' : ''}`} onClick={() => setPower('auto')} disabled={!isDebug}>AUTO</button>
          <button className={`seg-btn${powerState === 'on' ? ' seg-on' : powerState === 'off' ? ' seg-off' : ''}`} onClick={() => setPower(powerState === 'on' ? 'off' : 'on')} disabled={!isDebug}>{powerState === 'off' ? 'OFF' : 'ON'}</button>
        </div>
        <span className="seg-lbl seg-lbl-gap">POE</span>
        <div className="seg-group">
          <button className={`seg-btn${poeState === 'auto' ? ' seg-auto-on' : ''}`} onClick={() => setPoe('auto')} disabled={!isDebug}>AUTO</button>
          <button className={`seg-btn${poeState === 'on' ? ' seg-on' : poeState === 'off' ? ' seg-off' : ''}`} onClick={() => setPoe(poeState === 'on' ? 'off' : 'on')} disabled={!isDebug}>{poeState === 'off' ? 'OFF' : 'ON'}</button>
        </div>
        <button className="btn btn-reboot seg-lbl-gap" onClick={reboot} disabled={!isDebug}>⟳  REBOOT</button>
      </div>

      {/* SERIAL + DEBUG LEVEL */}
      <div className="controls-bar">
        <div className="ctrl-group">
          <span className="ctrl-lbl">SERIAL NO.</span>
          <div className="serial-input-wrap">
            <input
              className="ctrl-input"
              value={serial}
              maxLength={10}
              onChange={e => setSerial(e.target.value.replace(/\D/g, ''))}
              placeholder="_ _ _ _ _ _ _ _ _ _"
            />
            <svg className="scan-icon" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2">
              <path d="M3 7V5a2 2 0 0 1 2-2h2M3 17v2a2 2 0 0 0 2 2h2M21 7V5a2 2 0 0 0-2-2h-2M21 17v2a2 2 0 0 1-2 2h-2M9 6h6M9 18h6M9 12h6"/>
            </svg>
          </div>
        </div>

        <div className="ctrl-group status-group">
          <span className="ctrl-lbl">STATUS</span>
          <span className={`status-led ${busy ? 'led-running' : 'led-idle'}`} />
          <span className="status-txt">{busy ? 'RUNNING' : 'READY'}</span>
        </div>
      </div>

      {/* STEP LED ROW */}
      <div className="step-row">
        {COMMANDS.map(item => (
          <div
            key={item.key}
            className={`step-cell${isDebug && !busy ? ' step-clickable' : ''}`}
            onClick={() => isDebug && !busy && validSerial() && callCommand(item.key, false)}
          >
            <span className={`led ${leds[item.key]}`} />
            <span className="step-lbl">{item.label}</span>
          </div>
        ))}
      </div>

      {/* PROGRESS */}
      <div className={`progress-track${!isDebug ? ' hidden' : ''}`}>
        <div className="progress-fill" style={{ width: `${progress}%` }} />
        <span className="progress-txt">{progress > 0 ? `${progress}%` : ''}</span>
      </div>

      {/* ACTION BUTTONS */}
      <div className="action-bar">
        <button className="btn btn-comm"   disabled={busy || isLocked} onClick={() => runSequence('commission')}>COMMISSION</button>
        <button className="btn btn-retest" disabled={busy || isLocked} onClick={() => runSequence('retest')}>TEST</button>
        <button className="btn btn-stop"   disabled={isLocked}         onClick={stop}>STOP</button>
        <button className="btn btn-clear"                  onClick={resetPanel}>CLEAR</button>
      </div>

      {isDebug && (
        <div className="log-drawer">
          <div className="log-head">
            <span className="log-title">DEBUG LOG</span>
            <span className={`log-state ${logConnected ? 'log-up' : 'log-down'}`}>
              {logConnected ? 'LIVE' : 'DOWN'}
            </span>
            {logError && <span className="log-error">{logError}</span>}
            <div className="log-level-wrap">
              <span className="log-level-lbl">DEBUG LVL</span>
              <select className="log-level-select" value={debug} onChange={e => setDebug(e.target.value)}>
                <option value="0">INFO</option>
                <option value="1">DEBUG</option>
                <option value="2">TRACE</option>
              </select>
            </div>
            <button className="log-btn" onClick={downloadLogs} disabled={logLines.length === 0}>DOWNLOAD</button>
            <button className="log-btn" onClick={() => setLogPaused(p => !p)}>{logPaused ? 'RESUME' : 'PAUSE'}</button>
            <button className="log-btn" onClick={() => setLogLines([])}>CLEAR</button>
          </div>
          <div className="log-body" ref={logViewportRef}>
            {logLines.length === 0 ? (
              <div className="log-empty">No log lines</div>
            ) : (
              logLines.map((line, idx) => (
                <div className="log-line" key={`${idx}-${line.slice(0, 16)}`}>{line}</div>
              ))
            )}
          </div>
        </div>
      )}

      {changePinModal && (
        <div className="pin-overlay" onClick={() => setChangePinModal(false)}>
          <div className="pin-box" onClick={e => e.stopPropagation()}>
            <div className="pin-title">{newPinStep === 1 ? `ENTER NEW ${changePinMode.toUpperCase()} PIN` : `CONFIRM ${changePinMode.toUpperCase()} PIN`}</div>
            <div className={`pin-display${pinError ? ' pin-error' : ''}`}>
              {'●'.repeat(newPin.length) || '—'}
            </div>
            <div className="pin-pad">
              {[1,2,3,4,5,6,7,8,9,'',0,'⌫'].map((k, i) => (
                <button key={i} className={`pin-key${k === '' ? ' pin-key-blank' : ''}`}
                  onClick={() => k === '⌫' ? setNewPin(p => p.slice(0,-1)) : k !== '' && changePinPress(String(k))}>
                  {k}
                </button>
              ))}
            </div>
            <button className="pin-cancel" onClick={() => setChangePinModal(false)}>CANCEL</button>
          </div>
        </div>
      )}

      {pinModal && (
        <div className="pin-overlay" onClick={() => setPinModal(false)}>
          <div className="pin-box" onClick={e => e.stopPropagation()}>
            <div className="pin-title">ENTER {pinTargetMode.toUpperCase()} PIN</div>
            <div className={`pin-display${pinError ? ' pin-error' : ''}`}>
              {'●'.repeat(pinEntry.length) || '—'}
            </div>
            <div className="pin-pad">
              {[1,2,3,4,5,6,7,8,9,'',0,'⌫'].map((k, i) => (
                <button key={i} className={`pin-key${k === '' ? ' pin-key-blank' : ''}`}
                  onClick={() => k === '⌫' ? setPinEntry(p => p.slice(0,-1)) : k !== '' && pinPress(String(k))}>
                  {k}
                </button>
              ))}
            </div>
            <button className="pin-cancel" onClick={() => setPinModal(false)}>CANCEL</button>
          </div>
        </div>
      )}

      {versionModal && (
        <div className="version-overlay" onClick={() => setVersionModal(false)}>
          <div className="version-box" onClick={e => e.stopPropagation()}>
            <div className="version-title">SYSTEM VERSIONS</div>
            <div className="version-row"><span>React</span><strong>{reactVersion}</strong></div>
            <div className="version-row"><span>Snap</span><strong>{snapVersion}</strong></div>
            <div className="version-row"><span>FW to Flash</span><strong>{fwVersion}</strong></div>
            <button className="version-close" onClick={() => setVersionModal(false)}>CLOSE</button>
          </div>
        </div>
      )}

    </div>
  );
}
