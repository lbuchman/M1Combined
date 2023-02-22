#ifndef COMMAND_HH
#define COMMAND_HH

void setgpio(int arg_cnt, char **args);
void getgpio(int arg_cnt, char **args);
void confgpio(int arg_cnt, char **args);
void pulsegpio(int arg_cnt, char **args);
void ddrtest(int arg_cnt, char **args);
void ddradrbus(int arg_cnt, char **args);
void ddrdatbus(int arg_cnt, char **args);

#endif
