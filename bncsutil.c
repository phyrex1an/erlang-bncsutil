#define MUTIL_CPU_X86 1

#include "erl_nif.h"
#include "bncsutil/bncsutil.h"
#include <stdio.h>

static int my_enif_list_size(ErlNifEnv* env, ERL_NIF_TERM list)
{
  ERL_NIF_TERM head, tail, nexttail;
  int size = 0;
  tail = list;
  while(enif_get_list_cell(env, tail, &head, &nexttail))
  {
    tail = nexttail;
    size = size+1;
  }
  return size;
}

static char* my_enif_get_string(ErlNifEnv *env, ERL_NIF_TERM list)
{
  char *buf;
  ERL_NIF_TERM head, tail, nexttail;
  int size=my_enif_list_size(env, list);
  int cell;

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
  // {ok, PublicValue, Product, Hash}
  return enif_make_tuple(env, 4,
			 enif_make_atom(env, "ok"),
			 enif_make_int(env, public_value),
			 enif_make_int(env, product),
			 enif_make_string(env, hash_buffer));
}

static ERL_NIF_TERM nif_extract_mpq_number(ErlNifEnv* env, ERL_NIF_TERM mpq_name_t)
{
  char *mpq_name = my_enif_get_string(env, mpq_name_t);
  if (!mpq_name)
  {
    return enif_make_badarg(env);
  }
  
  int response = extractMPQNumber(mpq_name);
  enif_free(env, mpq_name);
  if (response == -1)
  {
    return enif_make_tuple(env, 2,
			   enif_make_atom(env, "error"),
			   enif_make_string(env, "Extraction failure"));
  }
  return enif_make_tuple(env, 2,
			 enif_make_atom(env, "ok"),
			 enif_make_int(env, response));
}

static ERL_NIF_TERM nif_check_revision(ErlNifEnv* env, 
				       ERL_NIF_TERM value_string_t,
				       ERL_NIF_TERM files_t,
				       ERL_NIF_TERM mpq_number_t)
{
  char *value_string;
  char **files;
  int num_files;
  int mpq_number;

  if (!enif_get_int(env, mpq_number_t, &mpq_number))
  {
    return enif_make_badarg(env);
  }

  value_string = my_enif_get_string(env, value_string_t);
  if (!value_string)
  {
    return enif_make_badarg(env);
  }

  num_files = my_enif_list_size(env, files_t);
  if (num_files>0)
  {
    files = enif_alloc(env, sizeof(char*)*num_files);
    if (!files)
    {
      enif_free(env, value_string);
      return enif_make_badarg(env);
    }
    int i=0;
    ERL_NIF_TERM tail, head, nexttail;
    tail = files_t;
    while(enif_get_list_cell(env, tail, &head, &nexttail))
    {
      tail = nexttail;
      if (!(files[i] = my_enif_get_string(env, head)))
      {
	enif_free(env, value_string);
	while(i>0)
	{
	  i--;
	  enif_free(env, files[i]);
	}
	enif_free(env, files);
	return enif_make_badarg(env);
      }
      i++;
    }
  }

  unsigned long checksum;
  int result;

  result = checkRevision(value_string, (const char**) files, num_files, mpq_number, &checksum);
  
  enif_free(env, value_string);
  int i;
  for(i=0; i < num_files; i++)
  {
    enif_free(env, files[i]);
  }
  enif_free(env, files);
  if (!result)
  {
    return enif_make_tuple(env, 2,
			   enif_make_atom(env, "error"),
			   enif_make_string(env, "Revision check failed"));
  }
  return enif_make_tuple(env, 2,
			 enif_make_atom(env, "ok"),
			 enif_make_ulong(env, checksum));
}

static ERL_NIF_TERM nif_get_exe_version(ErlNifEnv* env, 
					ERL_NIF_TERM file_name_t, 
					ERL_NIF_TERM platform_t)
{
  char *file_name;
  int platform;
  
  if (!enif_get_int(env, platform_t, &platform))
  {
    return enif_make_badarg(env);
  }
  
  file_name = my_enif_get_string(env, file_name_t);
  if (!file_name)
  {
    return enif_make_badarg(env);
  }

  char exe_info[100];
  uint32_t version;

  int real_size = getExeInfo(file_name, exe_info, 100, &version, platform);
  ERL_NIF_TERM exe_info_t;
  if (real_size>100)
  {
    char *exe_info_d = enif_alloc(env, real_size);
    if (!getExeInfo(file_name, exe_info, real_size, &version, platform))
    {
      enif_free(env, exe_info_d);
      enif_free(env, file_name);
      return enif_make_tuple(env, 2,
			     enif_make_atom(env, "error"),
			     enif_make_string(env, "ExeInfo retry failed"));
    }
    exe_info_t = enif_make_string(env, exe_info_d);
    enif_free(env, exe_info_d);
  }
  else
  {
    if (!real_size)
    {
      enif_free(env, file_name);
      return enif_make_tuple(env, 2,
			     enif_make_atom(env, "error"),
			     enif_make_string(env, "ExeInfo failed"));
    }
    exe_info_t = enif_make_string(env, exe_info);
  }
  enif_free(env, file_name);
  return enif_make_tuple(env, 3,
			 enif_make_atom(env, "ok"),
			 exe_info_t,
			 enif_make_int(env, version));
}

static ErlNifFunc nif_funcs[] = 
{
  {"echo", 1, (void*)nif_echo},
  {"hash_cdkey", 3, (void*)nif_hash_cdkey},
  {"extract_mpq_number", 1, (void*)nif_extract_mpq_number},
  {"check_revision", 3, (void*)nif_check_revision},
  {"get_exe_version", 2, (void*)nif_get_exe_version}
};
ERL_NIF_INIT(bncsutil,nif_funcs,NULL,NULL,NULL,NULL)
