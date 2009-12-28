#define MUTIL_CPU_X86 1

#include "erl_nif.h"
#include "bncsutil/bncsutil.h"
#include <stdio.h>

static char* my_enif_get_string(ErlNifEnv *env, ERL_NIF_TERM list)
{
  char *buf;
  ERL_NIF_TERM head, tail, nexttail;
  int size=0;
  int cell;

  tail = list;
  while(enif_get_list_cell(env, tail, &head, &nexttail))
  {
    tail = nexttail;
    if (!enif_get_int(env, head, &cell)) 
    {
      return 0;
    }
    size = size+1;
  }

  if (!(buf = (char*) enif_alloc(env, size+1)))
  {
    return NULL;
  }
  
  tail = list;
  size=0;
  while(enif_get_list_cell(env, tail, &head, &nexttail))
  {
    tail = nexttail;
    if (!enif_get_int(env, head, &cell)) 
    {
      // should never happen, this is checked above
      enif_free(env, buf);
      return NULL;
    }
    buf[size] = (char)cell;
    size++;
  }
  
  buf[size]='\0';
  return buf;
}

void debug(char* msg)
{
  FILE* dbg = fopen("debug.txt", "a");
  fprintf(dbg, "%s\n", msg);
  fclose(dbg);
}


static ERL_NIF_TERM nif_echo(ErlNifEnv* env, ERL_NIF_TERM str)
{
  ERL_NIF_TERM resp;
  char* e;
  if (!(e = my_enif_get_string(env, str)))
  {
    return enif_make_badarg(env);
  }
  resp = enif_make_string(env, e);
  enif_free(env, e);
  return resp;
}


static ERL_NIF_TERM nif_hash_cdkey(ErlNifEnv* env, ERL_NIF_TERM cd_key_t, ERL_NIF_TERM client_token_t, ERL_NIF_TERM server_token_t)
{
  // kd_quick(const char* cd_key, uint32_t client_token,
  //				   uint32_t server_token, uint32_t* public_value,
  //				   uint32_t* product, char* hash_buffer, size_t buffer_len)

  // Params
  unsigned long client_token;
  if (!enif_get_ulong(env, client_token_t, &client_token))
  {
    return enif_make_badarg(env);
  }
  unsigned long server_token;
  if (!enif_get_ulong(env, server_token_t, &server_token))
  {
    return enif_make_badarg(env);
  }
  char* cd_key = my_enif_get_string(env, cd_key_t);
  if (!cd_key)
  {
    return enif_make_badarg(env);
  }

  // Response
  unsigned int public_value;
  unsigned int product;
  char hash_buffer[4*5+1];

  hash_buffer[4*5]='\0';

  // Other
  size_t buffer_len = 4*5;
  int result;

  result = kd_quick(cd_key, client_token, server_token, &public_value, &product, hash_buffer, buffer_len);
  enif_free(env, cd_key);
  if(!result)
  {
    // {error, "error when decoding cdkey"}
    return enif_make_tuple(env, 2, 
			   enif_make_atom(env, "error"), 
			   enif_make_string(env, "error when decoding cdkey"));
  }
  // {ok, public_value, product, hash}
  return enif_make_tuple(env, 4,
			 enif_make_atom(env, "ok"),
			 enif_make_int(env, public_value),
			 enif_make_int(env, product),
			 enif_make_string(env, hash_buffer));
  }


static ErlNifFunc nif_funcs[] = 
{
  {"echo", 1, (void*)nif_echo},
  {"hash_cdkey", 3, (void*)nif_hash_cdkey}
};
ERL_NIF_INIT(bncsutil,nif_funcs,NULL,NULL,NULL,NULL)
