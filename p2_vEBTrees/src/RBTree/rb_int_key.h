#ifndef RBINTKEY
#define RBINTKEY

int Compare(const void* a, const void* b);
void DestroyKey(void* a);
void DestroyInfo(void* a);
void PrintKey(void* a);
void PrintInfo(void* a);

#endif