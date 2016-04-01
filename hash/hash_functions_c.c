#include <stdint.h>
#include <stddef.h>
#include <stdio.h>


uint32_t get_UINT32_MAX()
{
  //   printf("UINT32_MAX = %u\n", UINT32_MAX);
  return UINT32_MAX;
}

// Fowler–Noll–Vo hash function
// https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
// http://www.isthe.com/chongo/tech/comp/fnv/
// http://stackoverflow.com/questions/34595/what-is-a-good-hash-function
uint32_t fnv1_32_hash (char* key, size_t len)
{
  uint32_t hash = 2166136261;
  size_t i;

  for ( i = 0; i < len; i++ )
    hash = ( hash * 16777619 ) ^ key[i];

  return hash;
}

uint32_t  fnv1a_32_hash( char *key, size_t len ) {
  uint32_t  hash = 0x811c9dc5;
  size_t i;

  for ( i = 0; i < len; i++ )
    hash = ( hash ^ key[i] ) * 0x01000193;

  return hash;
}
// http://www.isthe.com/chongo/tech/comp/fnv/
uint64_t fnv1a_64_hash( char *key, size_t len ) {
  uint64_t hash = 0xcbf29ce484222325ULL;
  size_t i;

  for ( i = 0; i < len; i++ )
    hash = ( hash ^ key[i] ) * 0x100000001b3ULL;

  return hash;
}

// https://en.wikipedia.org/wiki/Jenkins_hash_function
// one-at-a-time
// http://www.burtleburtle.net/bob/hash/doobs.html
uint32_t jenkins_one_at_a_time_hash(char *key, size_t len)
{
  uint32_t hash = 0;
  size_t i;
  for(i = 0; i < len; i++)
  {
    hash += key[i];
    hash += (hash << 10);
    hash ^= (hash >> 6);
  }
  hash += (hash << 3);
  hash ^= (hash >> 11);
  hash += (hash << 15);
  return hash;
}

// this algorithm was created for sdbm (a public-domain reimplementation of ndbm) database library.
uint32_t sdbm_hash(char* key, size_t len)
{
  uint32_t hash = 0; // 5381; //
  size_t i = 0;

  for(i = 0; i < len; i++)
  {
    hash = key[i] + (hash << 6) + (hash << 16) - hash;
  }

  return hash;
}

// Bernstein's hash
uint32_t djbx_hash(char* key, size_t len) {
  uint32_t hash = 5381;
  size_t i;
  for( i = 0; i < len; ++i)
    hash = 33 * hash + key[i];
  return hash ^ (hash >> 16);
}
// Bernstein's hash
uint32_t djb_hash (char *key, size_t len)
{

  uint32_t hash = 5381;
  size_t i;

  for ( i = 0; i < len; i++ )
    hash = 33 * hash + key[i];

  return hash;
}
// Bernstein's hash
uint32_t djb2_hash (char *key, size_t len)
{
  uint32_t hash = 5381;
  size_t i;

  for ( i = 0; i < len; i++ )
    hash = 33 * hash ^ key[i];

  return hash;
}
// Bernstein's hash
uint32_t djb3_hash(char* key, size_t len)
{
  uint32_t hash = 5381;
  size_t i;

  for (i = 0; i < len; i++) {
    hash = ((hash << 5) + hash) + key[i];
  }

  return hash;
}

// class of simple hash function for hashing character strings named shift- add-xor (SAX)
uint32_t sax_hash (char* key, size_t len)
{
  uint32_t hash = 0;
  size_t i;

  for ( i = 0; i < len; i++ )
    hash ^= ( hash << 5 ) + ( hash >> 2 ) + key[i];

  return hash;
}

// published hash algorithm used in the UNIX ELF format for object files.
uint32_t elf_hash (char* key, size_t len)
{

  uint32_t hash = 0, g;
  size_t i;

  for ( i = 0; i < len; i++ ) {
    hash = ( hash << 4 ) + key[i];
    g = hash & 0xf0000000L;

    if ( g != 0 )
      hash ^= g >> 24;

    hash &= ~g;
  }

  return hash;
}

// http://stackoverflow.com/questions/664014/what-integer-hash-function-are-good-that-accepts-an-integer-hash-key Thomas Mueller
uint32_t tmueller_hash(char* key, size_t len)
{
  uint32_t hash = 0;
  size_t i;

  for ( i = 0; i < len; i++ ) {
    hash += key[i];
    hash = ((hash >> 16) ^ hash) * 0x45d9f3b;
    hash = ((hash >> 16) ^ hash) * 0x45d9f3b;
    hash = ((hash >> 16) ^ hash);
  }
  return hash;
}

//  Donald E. Knuth .
// The algorithm was presented in "The Art Of Computer Programming" Volume 3, Chapter 6.4, Topic: Sorting and search.
uint32_t knuth_hash(char* key, size_t len)
{
  uint32_t hash = len;
  size_t i = 0;

  for(i = 0; i < len; i++)
  {
    hash = ((hash << 5) ^ (hash >> 27)) ^ key[i];
  }
  return hash;
}

// AP Hash is an algorithm invented by Arash Partow
uint32_t apartow_hash(char* key, size_t len)
{
  uint32_t hash = 0xAAAAAAAA;
  size_t i = 0;

  for(i = 0; i < len; i++)
  {
    hash ^= ((i & 1) == 0) ? (  (hash <<  7) ^ key[i] * (hash >> 3)) :
    (~((hash << 11) + ( key[i] ^ (hash >> 5))));
  }

  return hash;
}

// simple hash function taken from Bruno Preiss's book:
// Data Structures and Algorithms with Object-Oriented Design Patterns in Java,'' John Wiley & Sons, 2000
uint32_t bpreiss_hash(char* key, size_t len)
{
  uint32_t hash = 0;
  size_t i = 0;

  for(i = 0; i < len; i++)
  {
    hash = hash << 7 ^ key[i];
  }

  return hash;
}


/*
uint32_t jsw_hash(char *key, size_t len)
{

  uint32_t hash = 16777551;
  uint32_t i;

  for (i = 0; i < len; i++)
  {
    hash = (hash << 1 | h >> 31) ^ tab[key[i]];
  }

  return h;
}*/