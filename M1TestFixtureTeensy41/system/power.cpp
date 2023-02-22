#include <power.h>
#include <logger.hpp>


extern "C" uint32_t set_arm_clock(uint32_t frequency);

extern Scheduler *pts;
static constexpr int FullSpeedClock = 600000000;
static constexpr int IdleSpeedClock =  66000000;
static constexpr int IdleInterval = TASK_SECOND * 30;
Task* pIdelWatchdog;
bool initialized = false;

void powerSetFullSpeed() {
    if(!initialized) {
        return;
    }

    if(F_CPU_ACTUAL != FullSpeedClock) {
        set_arm_clock(FullSpeedClock);
        plogger->info(plogger->printHeader, (char*) __FILE__, __LINE__, "Enable Full Speed Clock = %dMhz", F_CPU_ACTUAL / 1000000);
    }

    pIdelWatchdog->disable();
    pIdelWatchdog->restartDelayed(IdleInterval);

}

void powerMngBegin() {
    initialized = true;
    plogger->info(plogger->printHeader, (char*) __FILE__, __LINE__, "Starting power management module");
    pIdelWatchdog = new Task(TASK_IMMEDIATE, TASK_ONCE, "CPU Idle Task", [](void) -> void {
        if(F_CPU_ACTUAL != IdleSpeedClock) {
            set_arm_clock(IdleSpeedClock);
            plogger->info(plogger->printHeader, (char*) __FILE__, __LINE__, "Set IDLE cpu clock = %dMhz", F_CPU_ACTUAL / 1000000);
        }
    }, pts, false, NULL, NULL);
    pIdelWatchdog->restartDelayed(IdleInterval);
}
