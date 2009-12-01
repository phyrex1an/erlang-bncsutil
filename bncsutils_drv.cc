#define MUTIL_CPU_X86 1

#include <epapi.h>
#include "bncsutil/bncsutil.h"
#include <iostream>
#include <fstream>

using namespace std;
#define MESSAGE_ECHO 1
#define MESSAGE_ECHO_RESPONSE 2

bool drv_echo(MsgHandler *messageHandler, Msg *message)
{
  char *e;
  message->getParam(0, &e);
  messageHandler->send(MESSAGE_ECHO_RESPONSE, e);
  return true;
}

bool drv_hashcdkey(MsgHandler *messageHandler, Msg *message)
{
  

  return true;
}

int main(int argc, char **argv)
{
  PktHandler *packetHandler = new PktHandler();
  MsgHandler *messageHandler = new MsgHandler(packetHandler);

  // Debug util
  messageHandler->registerType(MESSAGE_ECHO, "echo", "s" );
  messageHandler->registerType(MESSAGE_ECHO_RESPONSE, "echo_response", "s" );

  // 

  do 
  {
    Msg *message;
    DBGLOG(LOG_INFO,"main - receiving");
    int result = messageHandler->rx(&message);

    if (result)
    {
      DBGLOG(LOG_ERR,"main - error: %s", messageHandler->strerror());
      break;
    }

    char *e;

    switch (message->getType())
    {
    case MESSAGE_ECHO:
      drv_echo(messageHandler, message);
      break;
    default:
      break;
    }
    fflush(stdout);
    DBGLOG(LOG_INFO, "main - finished sending");
    delete message;
  } while (1);
  return 0;
}
