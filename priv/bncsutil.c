#define MUTIL_CPU_X86 1

#include "erl_nif.h"
#include "bncsutil/bncsutil.h"

static ERL_NIF_TERM my_enif_make_error(ErlNifEnv *env, char *msg)
{
  return enif_make_tuple(env, 2,
			 enif_make_atom(env, "error"),
			 enif_make_string(env, msg, ERL_NIF_LATIN1));
}

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
  int size=my_enif_list_size(env, list);

  if (!(buf = (char*) enif_alloc(env, size+1)))
  {
    return NULL;
  }
  if (enif_get_string(env, list, buf, size+1, ERL_NIF_LATIN1)<1)
  {
    enif_free(env, buf);
    return NULL;
  }
  return buf;
}


static ERL_NIF_TERM nif_hash_cdkey(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  // kd_quick(const char* cd_key, uint32_t client_token,
  //				   uint32_t server_token, uint32_t* public_value,
  //				   uint32_t* product, char* hash_buffer, size_t buffer_len)

  // Params
  unsigned long client_token;
  if (!enif_get_ulong(env, argv[1], &client_token))
  {
    return enif_make_badarg(env);
  }
  unsigned long server_token;
  if (!enif_get_ulong(env, argv[2], &server_token))
  {
    return enif_make_badarg(env);
  }
  char* cd_key = my_enif_get_string(env, argv[0]);
  if (!cd_key)
  {
    return enif_make_badarg(env);
  }

  // Response
  unsigned int public_value;
  unsigned int product;
  
  ErlNifBinary hash;
  if (!enif_alloc_binary(env, 20, &hash))
  {
    enif_free(env, cd_key);
    return my_enif_make_error(env, "Failed to allocate binary");
  }

  // Other
  size_t buffer_len = 4*5;
  int result;

  result = kd_quick(cd_key, client_token, server_token, &public_value, &product, (char*)hash.data, buffer_len);
  enif_free(env, cd_key);
  if(!result)
  {
    // {error, "error when decoding cdkey"}
    return my_enif_make_error(env, "error when decoding cdkey");
  }
  // {ok, PublicValue, Product, Hash}
  return enif_make_tuple(env, 4,
			 enif_make_atom(env, "ok"),
			 enif_make_int(env, public_value),
			 enif_make_int(env, product),
			 enif_make_binary(env, &hash));
}

static ERL_NIF_TERM nif_extract_mpq_number(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  char *mpq_name = my_enif_get_string(env, argv[0]);
  if (!mpq_name)
  {
    return enif_make_badarg(env);
  }
  
  int response = extractMPQNumber(mpq_name);
  enif_free(env, mpq_name);
  if (response == -1)
  {
    return my_enif_make_error(env, "Extraction failure");
  }
  return enif_make_tuple(env, 2,
			 enif_make_atom(env, "ok"),
			 enif_make_int(env, response));
}

static ERL_NIF_TERM nif_check_revision(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  char *value_string;
  char **files;
  int num_files;
  int mpq_number;

  if (!enif_get_int(env, argv[2], &mpq_number))
  {
    return enif_make_badarg(env);
  }

  value_string = my_enif_get_string(env, argv[0]);
  if (!value_string)
  {
    return enif_make_badarg(env);
  }

  num_files = my_enif_list_size(env, argv[1]);
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
    tail = argv[1];
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
    return my_enif_make_error(env, "Revision check failed");
  }
  return enif_make_tuple(env, 2,
			 enif_make_atom(env, "ok"),
			 enif_make_ulong(env, checksum));
}

static ERL_NIF_TERM nif_get_exe_version(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  char *file_name;
  int platform;
  
  if (!enif_get_int(env, argv[1], &platform))
  {
    return enif_make_badarg(env);
  }
  
  file_name = my_enif_get_string(env, argv[0]);
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
      return my_enif_make_error(env, "ExeInfo retry failed");
    }
    exe_info_t = enif_make_string(env, exe_info_d, ERL_NIF_LATIN1);
    enif_free(env, exe_info_d);
  }
  else
  {
    if (!real_size)
    {
      enif_free(env, file_name);
      return my_enif_make_error(env, "ExeInfo failed");
    }
    exe_info_t = enif_make_string(env, exe_info, ERL_NIF_LATIN1);
  }
  enif_free(env, file_name);
  return enif_make_tuple(env, 3,
			 enif_make_atom(env, "ok"),
			 exe_info_t,
			 enif_make_int(env, version));
}

static ERL_NIF_TERM nif_nls_init(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
  char *username;
  char *password;

  username = my_enif_get_string(env, argv[0]);
  if(!username)
  {
    return enif_make_badarg(env);
  }
  password = my_enif_get_string(env, argv[1]);
  if (!password)
  {
    enif_free(env, username);
    return enif_make_badarg(env);
  }
  
  nls_t *nls = nls_init(username, password);
  enif_free(env, username);
  enif_free(env, password);
  
  return enif_make_tuple(env, 2,
			 enif_make_atom(env, "ok"),
			 enif_make_ulong(env, (long)nls));
}

static ERL_NIF_TERM nif_nls_free(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
  unsigned long nls;
  if (!enif_get_ulong(env, argv[0], &nls))
  {
    return enif_make_badarg(env);
  }
  nls_free((void *)nls);
  return enif_make_tuple(env, 1,
			 enif_make_atom(env, "ok"));
}

static ERL_NIF_TERM nif_nls_get_S(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
  
  unsigned long nls;
  ErlNifBinary B;
  ErlNifBinary salt;

  if (!enif_get_ulong(env, argv[0], &nls))
  {
    return enif_make_badarg(env);
  }

  if (!enif_inspect_binary(env, argv[1], &B))
  {
    return enif_make_badarg(env);
  }

  if (!enif_inspect_binary(env, argv[2], &salt))
  {
    enif_release_binary(env, &B);
    return enif_make_badarg(env);
  }

  ErlNifBinary S;
  if (!enif_alloc_binary(env, 32, &S))
  {
    enif_release_binary(env, &B);
    enif_release_binary(env, &salt);
    return my_enif_make_error(env, "Failed to allocate binary");
  }

  nls_get_S((void *)nls, 
	    (char *)S.data, 
	    (char *)B.data, 
	    (char *)salt.data);
  
  enif_release_binary(env, &B);
  enif_release_binary(env, &salt);
  return enif_make_tuple(env, 2,
			 enif_make_atom(env, "ok"),
			 enif_make_binary(env, &S));
}

static ERL_NIF_TERM nif_nls_get_v(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
  unsigned long nls;
  ErlNifBinary salt;
  
  if (!enif_get_ulong(env, argv[0], &nls))
  {
    return enif_make_badarg(env);
  }

  if (!enif_inspect_binary(env, argv[1], &salt))
  {
    return enif_make_badarg(env);
  }

  ErlNifBinary v;
  if (!enif_alloc_binary(env, 32, &v))
  {
    enif_release_binary(env, &salt);
    return my_enif_make_error(env, "Failed to allocate binary");
  }

  nls_get_v((void *)nls,
	    (char *)v.data,
	    (char *)salt.data);
  
  enif_release_binary(env, &salt);

  return enif_make_tuple(env, 2,
			 enif_make_atom(env, "ok"),
			 enif_make_binary(env, &v));
}

static ERL_NIF_TERM nif_nls_get_A(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
  unsigned long nls;
  
  if (!enif_get_ulong(env, argv[0], &nls))
  {
    return enif_make_badarg(env);
  }

  ErlNifBinary A;
  if (!enif_alloc_binary(env, 32, &A))
  {
    return my_enif_make_error(env, "Failed to allocate binary");
  }

  nls_get_A((void *)nls,
	    (char *)A.data);

  return enif_make_tuple(env, 2,
			 enif_make_atom(env, "ok"),
			 enif_make_binary(env, &A));
}

static ERL_NIF_TERM nif_nls_get_K(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
  unsigned long nls;
  ErlNifBinary S;
  
  if (!enif_get_ulong(env, argv[0], &nls))
  {
    return enif_make_badarg(env);
  }

  if (!enif_inspect_binary(env, argv[1], &S))
  {
    return enif_make_badarg(env);
  }

  ErlNifBinary K;
  if (!enif_alloc_binary(env, 40, &K))
  {
    enif_release_binary(env, &S);
    return my_enif_make_error(env, "Failed to allocate binary");
  }

  nls_get_K((void *)nls,
	    (char *)K.data,
	    (char *)S.data);
  
  enif_release_binary(env, &S);

  return enif_make_tuple(env, 2,
			 enif_make_atom(env, "ok"),
			 enif_make_binary(env, &K));
}

static ERL_NIF_TERM nif_nls_get_M1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
  unsigned long nls;
  ErlNifBinary B;
  ErlNifBinary salt;
  
  if (!enif_get_ulong(env, argv[0], &nls))
  {
    return enif_make_badarg(env);
  }

  if (!enif_inspect_binary(env, argv[1], &B))
  {
    return enif_make_badarg(env);
  }

  if (!enif_inspect_binary(env, argv[2], &salt))
  {
    enif_release_binary(env, &B);
    return enif_make_badarg(env);
  }

  ErlNifBinary M1;
  if (!enif_alloc_binary(env, 20, &M1))
  {
    enif_release_binary(env, &B);
    enif_release_binary(env, &salt);
    return my_enif_make_error(env, "Failed to allocate binary");
  }
  
  nls_get_M1((void *)nls,
	     (char *)M1.data,
	     (char *)B.data,
	     (char *)salt.data);
  
  enif_release_binary(env, &B);
  enif_release_binary(env, &salt);
  
  return enif_make_tuple(env, 2,
			 enif_make_atom(env, "ok"),
			 enif_make_binary(env, &M1));
}

static ERL_NIF_TERM nif_nls_check_M2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{

  unsigned long nls;
  ErlNifBinary M2;

  if (!enif_get_ulong(env, argv[0], &nls))
  {
    return enif_make_badarg(env);
  }
  
  if (!enif_inspect_binary(env, argv[1], &M2))
  {
    return enif_make_badarg(env);
  }

  int response = 
    nls_check_M2((void*)nls, (char*)M2.data, NULL, NULL);
  
  enif_release_binary(env, &M2);
  if (response)
  {
    return enif_make_tuple(env, 1,
			   enif_make_atom(env, "ok"));
  }
  else
  {
    return enif_make_tuple(env, 1,
			   enif_make_atom(env, "fail"));
  }
}

static ERL_NIF_TERM nif_nls_check_signature(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{

  unsigned long address;
  ErlNifBinary signature_raw;

  if (!enif_get_ulong(env, argv[0], &address))
  {
    return enif_make_badarg(env);
  }
  
  if (!enif_inspect_binary(env, argv[1], &signature_raw))
  {
    return enif_make_badarg(env);
  }

  int response = 
    nls_check_signature((unsigned int)address, 
			(char *)signature_raw.data);
  
  enif_release_binary(env, &signature_raw);
  if (response)
  {
    return enif_make_tuple(env, 1,
			   enif_make_atom(env, "ok"));
  }
  else
  {
    return enif_make_tuple(env, 1,
			   enif_make_atom(env, "fail"));
  }
}



static ErlNifFunc nif_funcs[] = 
{
  {"hash_cdkey", 3, (void*)nif_hash_cdkey},
  {"extract_mpq_number", 1, (void*)nif_extract_mpq_number},
  {"check_revision", 3, (void*)nif_check_revision},
  {"get_exe_version", 2, (void*)nif_get_exe_version},
  {"nls_init", 2, (void*)nif_nls_init},
  {"nls_free", 1, (void*)nif_nls_free},
  {"nls_get_S", 3, (void*)nif_nls_get_S},
  {"nls_get_v", 2, (void*)nif_nls_get_v},
  {"nls_get_A", 1, (void*)nif_nls_get_A},
  {"nls_get_K", 2, (void*)nif_nls_get_K},
  {"nls_get_M1",3, (void*)nif_nls_get_M1},
  {"nls_check_M2", 2, (void*)nif_nls_check_M2},
  {"nls_check_signature", 2, (void*)nif_nls_check_signature}
};
ERL_NIF_INIT(bncsutil,nif_funcs,NULL,NULL,NULL,NULL)
