#include <stdbool.h>
#include <stdint.h>



void ex3();

extern struct {  /* state */
  struct {  /* ex3 */
    struct {  /* ex3 */
      bool __channel_ach;
      bool __channel_ach_ready;
      bool __channel_bch;
      bool __channel_bch_ready;
      struct {  /* alice */
        int8_t x;
      } alice;
      struct {  /* bob */
        int8_t x;
      } bob;
    } ex3;
  } ex3;
} state;



