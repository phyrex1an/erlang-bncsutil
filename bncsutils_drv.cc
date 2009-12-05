#define MUTIL_CPU_X86 1

#include <epapi.h>
#include "bncsutil/bncsutil.h"
#include <iostream>
#include <fstream>

using namespace std;
#define MESSAGE_ECHO 1
#define MESSAGE_ECHO_RESPONSE 2
#define MESSAGE_ERROR_RESPONSE 3
#define MESSAGE_HASH_CDKEY 4
#define MESSAGE_HASH_CDKEY_RESPONSE 5

bool drv_echo(MsgHandler *messageHandler, Msg *message)
{
  char *e;
  message->getParam(0, &e);
  messageHandler->send(MESSAGE_ECHO_RESPONSE, e);
  return true;
}

bool drv_hash_cdkey(MsgHandler *messageHandler, Msg *message)
{
  // kd_quick(const char* cd_key, uint32_t client_token,
  //				   uint32_t server_token, uint32_t* public_value,
  //				   uint32_t* product, char* hash_buffer, size_t buffer_len)

  // Params
  char * cd_key;
  //uint32_t client_token;
  //uint32_t server_token;

  // Response
  uint32_t public_value;
  uint32_t product;
  char hash_buffer[4*5+1];

  hash_buffer[4*5]='\0';

  // Other
  long client_token_l;
  long server_token_l;
  size_t buffer_len = 4*5;
  int result;

  message->getParam(0, &cd_key);
  message->getParam(1, &client_token_l);
  message->getParam(2, &server_token_l);

  uint32_t client_token = (client_token_l);
  uint32_t server_token = (server_token_l);

  result = kd_quick(cd_key, client_token, server_token, &public_value, &product, hash_buffer, buffer_len);
  if(!result)
  {
    messageHandler->send(MESSAGE_ERROR_RESPONSE, "error when decoding cdkey");
    return false;
  }
  
  messageHandler->send(MESSAGE_HASH_CDKEY_RESPONSE, public_value, product, hash_buffer);

  return true;
}

int main(int argc, char **argv)
{
  PktHandler *packetHandler = new PktHandler();
  MsgHandler *messageHandler = new MsgHandler(packetHandler);

  // Debug util
  messageHandler->registerType(MESSAGE_ECHO, "echo", "s" );
  messageHandler->registerType(MESSAGE_ECHO_RESPONSE, "echo_response", "s" );

  messageHandler->registerType(MESSAGE_ERROR_RESPONSE, "error_response", "s");

  messageHandler->registerType(MESSAGE_HASH_CDKEY, "hash_cdkey", "sll");
  messageHandler->registerType(MESSAGE_HASH_CDKEY_RESPONSE, "hash_cdkey_response", "lls");

  // 

  do 
  {
    Msg *message;
    DBGLOG(LOG_INFO,"main - receiving");
    int result = messageHandler->rx(&message);

    if (result)
    {
      DBGLOG(LOG_INFO,"main - error: %s", messageHandler->strerror());
      break;
    }

    char *e;
    DBGLOG(LOG_INFO, "main -sending");
    switch (message->getType())
    {
    case MESSAGE_ECHO:
      drv_echo(messageHandler, message);
      break;
    case MESSAGE_HASH_CDKEY:
      drv_hash_cdkey(messageHandler, message);
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
