import { useRef, useState, useEffect } from 'react';

const COMMANDS = [
  { key: 'ict',         label: 'ICT'       },
  { key: 'progmac',    label: 'MAC PROG'  },
  { key: 'flash',      label: 'FLASH'     },
  { key: 'functest',   label: 'FUNC TEST' },
  { key: 'eeprom',     label: 'EEPROM'    },
  { key: 'pingM1apps', label: 'APPS CHK'  },
  { key: 'makelabel',  label: 'LABEL'     }
];

const PROGRESS = { ict:5, progmac:3, flash:40, functest:43, eeprom:5, pingM1apps:3, makelabel:8 };
const RETEST_SKIP = new Set(['flash']);
const API = localStorage.getItem('m1-api') || 'http://127.0.0.1:3300';

function initLeds() {
  return Object.fromEntries(COMMANDS.map(c => [c.key, 'idle']));
}

export default function App() {
  const [serial,   setSerial]   = useState('');
  const [debug,    setDebug]    = useState('1');
  const [appMode,  setAppMode]  = useState('production');
  const [busy,     setBusy]     = useState(false);
  const [leds,     setLeds]     = useState(initLeds);
  const [progress, setProgress] = useState(0);
  const [result,    setResult]    = useState(null);   // { ok, step, description }
  const [powerState, setPowerState] = useState('auto'); // 'auto', 'on', 'off'
  const [poeState,   setPoeState]   = useState('auto'); // 'auto', 'on', 'off'
  const [machineName, setMachineName] = useState('FC?');
  const [pinModal,   setPinModal]   = useState(false);
  const [pinEntry,   setPinEntry]   = useState('');
  const [pinError,   setPinError]   = useState(false);
  const [changePinModal, setChangePinModal] = useState(false);
  const [newPin,     setNewPin]     = useState('');
  const [newPinStep, setNewPinStep] = useState(1); // 1=enter new, 2=confirm
  const [newPinFirst, setNewPinFirst] = useState('');
  const stopRef = useRef(false);

  useEffect(() => {
    fetch(`${API}/config`)
      .then(r => r.json())
      .then(b => { if (b.machineName) setMachineName(b.machineName); })
      .catch(() => {});
  }, []);

  const isDebug = appMode === 'debug';

  function openChangePinModal() { setNewPin(''); setNewPinStep(1); setNewPinFirst(''); setChangePinModal(true); }

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
            const res = await fetch(`${API}/changepin`, {
              method: 'POST', headers: { 'Content-Type': 'application/json' },
              body: JSON.stringify({ pin: next })
            });
            const body = await res.json();
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

  function openPinModal() { setPinEntry(''); setPinError(false); setPinModal(true); }

  async function pinPress(d) {
    if (pinEntry.length >= 6) return;
    const next = pinEntry + d;
    setPinEntry(next);
    if (next.length >= 4) {
      try {
        const res = await fetch(`${API}/auth`, {
          method: 'POST', headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ pin: next })
        });
        const body = await res.json();
        if (body.status === 'OK') { setPinModal(false); setAppMode('debug'); }
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

  async function callCommand(key, isRetest) {
    const arg = { serial: serial.trim(), debug };
    if (key === 'ict') arg.cellBatTol = isRetest ? 'used' : 'new';
    setLed(key, 'running');
    try {
      const res  = await fetch(`${API}/command`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ command: key, argument: arg })
      });
      const body = await res.json();
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
      await fetch(`${API}/command`, {
        method: 'POST', headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ command: 'cleanup', argument: { serial: serial.trim() } })
      }).catch(() => {});
    }

    setResult(failed ? { ok: false, ...failed } : { ok: true, step: '', description: 'All tests passed' });
    setBusy(false);
  }

  function stop() { stopRef.current = true; setBusy(false); }

  function openLogs() { window.open('http://localhost:8080', '_blank', 'width=1100,height=700'); }

  async function setPower(newState) {
    setPowerState(newState);
    try {
      await fetch(`${API}/command`, {
        method: 'POST', headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ command: 'power', argument: { state: newState } })
      }).catch(() => {});
    } catch (err) {}
  }

  async function setPoe(newState) {
    setPoeState(newState);
    try {
      await fetch(`${API}/command`, {
        method: 'POST', headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ command: 'poe', argument: { state: newState } })
      }).catch(() => {});
    } catch (err) {}
  }

  async function reboot() {
    try {
      await fetch(`${API}/command`, {
        method: 'POST', headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ command: 'reboot', argument: {} })
      }).catch(() => {});
    } catch (err) {}
  }

  return (
    <div className="panel">

      {/* TITLE */}
      <div className="title-bar">
        <div className="honeywell-brand">
          <span className="honeywell-text">HONEYWELL</span>
        </div>
        <span className="title-text">MnPlus</span>
        <span className="machine-name">{machineName}</span>
        <div className="mode-wrap">
          <button className={`mode-btn ${!isDebug ? 'mode-on' : ''}`} onClick={() => { setAppMode('production'); setPowerState('auto'); setPoeState('auto'); setDebug('1'); }}>PRODUCTION</button>
          <button className={`mode-btn ${isDebug  ? 'mode-on' : ''}`} onClick={isDebug ? undefined : openPinModal}>DEBUG</button>
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

        <div className="ctrl-group" style={{visibility: isDebug ? 'visible' : 'hidden'}}>
            <span className="ctrl-lbl">DEBUG LVL</span>
            <select className="ctrl-select" value={debug} onChange={e => setDebug(e.target.value)}>
              <option value="0">D0</option>
              <option value="1">D1</option>
              <option value="2">D2</option>
            </select>
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
      <div className="progress-track">
        <div className="progress-fill" style={{ width: `${progress}%` }} />
        <span className="progress-txt">{progress > 0 ? `${progress}%` : ''}</span>
      </div>

      {/* RESULT DISPLAY */}
      <div className={`result-box ${result ? (result.ok ? 'result-pass' : 'result-fail') : 'result-idle'}`}>
        {!result && <span className="result-idle-txt">─</span>}
        {result && (
          <>
            <span className="result-verdict">{result.ok ? '✔  PASS' : '✘  FAIL'}</span>
            {result.description && <span className="result-desc">{result.description}</span>}
            {result.step && <span className="result-step">Failed step: {result.step}</span>}
          </>
        )}
      </div>

      {/* ACTION BUTTONS */}
      <div className="action-bar">
        <button className="btn btn-comm"   disabled={busy} onClick={() => runSequence('commission')}>COMMISSION</button>
        <button className="btn btn-retest" disabled={busy} onClick={() => runSequence('retest')}>RE-TEST</button>
        <button className="btn btn-stop"                   onClick={stop}>STOP</button>
        <button className="btn btn-clear"                  onClick={resetPanel}>CLEAR</button>
        <button className="btn btn-logs" onClick={openLogs} style={{visibility: isDebug ? 'visible' : 'hidden'}}>OPEN LOGS</button>
        <button className="btn btn-chgpin" onClick={openChangePinModal} style={{visibility: isDebug ? 'visible' : 'hidden', marginLeft:'auto'}}>CHANGE PIN</button>
      </div>

      {changePinModal && (
        <div className="pin-overlay" onClick={() => setChangePinModal(false)}>
          <div className="pin-box" onClick={e => e.stopPropagation()}>
            <div className="pin-title">{newPinStep === 1 ? 'ENTER NEW PIN' : 'CONFIRM NEW PIN'}</div>
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
            <div className="pin-title">SUPERVISOR ACCESS</div>
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

    </div>
  );
}
