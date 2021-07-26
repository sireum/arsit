#include <all.h>
#include <signal.h>

__attribute__((weak)) IS_7E8796 PACKAGE_NAME_RunRoundRobinDemo_schedule(STACK_FRAME_ONLY);

volatile sig_atomic_t shouldStop = 0;

void PACKAGE_NAME_RoundRobinProvider_getRoundRobinOrder(STACK_FRAME IS_7E8796 result) {

  if(PACKAGE_NAME_RunRoundRobinDemo_schedule) {
    IS_7E8796 order = PACKAGE_NAME_RunRoundRobinDemo_schedule();
    memcpy(result->value, order->value, sizeof(union art_Bridge) * order->size);
    result->size = order->size;
  } else {
    printf("RunRoundRobinDemo.schedule not found.  You'll need to supply your own order in C\n");
    exit(-1);
  }
}

void sigHandler(int signo) {
  shouldStop = 1;
}

Unit art_scheduling_roundrobin_RoundRobinExtensions_init(STACK_FRAME_ONLY){
  int sigs[] = {SIGINT, SIGTERM, SIGQUIT};
  for(int i = 0; i < sizeof(sigs) / sizeof(int); i++){
    if(signal(sigs[i], sigHandler) == SIG_ERR) {
      printf("Error occurred while setting signal handler for %i\n", sigs[i]);
      exit(-1);
    }
  }
}

B art_scheduling_roundrobin_RoundRobinExtensions_shouldStop(STACK_FRAME_ONLY){
    return shouldStop == 1;
}
