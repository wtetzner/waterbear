
.include "basic.s"

; Note that we use a big-endian layout for multibyte values

%macro add64mem %input
  add %input + 7
  st %acc + 7

  ld %acc + 6
  addc %input + 6
  st %acc + 6

  ld %acc + 5
  addc %input + 5
  st %acc + 5

  ld %acc + 4
  addc %input + 4
  st %acc + 4

  ld %acc + 3
  addc %input + 3
  st %acc + 3

  ld %acc + 2
  addc %input + 2
  st %acc + 2

  ld %acc + 1
  addc %input + 1
  st %acc + 1

  ld %acc
  addc %input
  st %acc
%end

%macro add32mem %acc, %input
  ld %acc + 3
  add %input + 3
  st %acc + 3

  ld %acc + 2
  addc %input + 2
  st %acc + 2

  ld %acc + 1
  addc %input + 1
  st %acc + 1

  ld %acc
  addc %input
  st %acc
%end

%macro add16mem %acc, %input
  ld %acc + 1
  add %input + 1
  st %acc + 1

  ld %acc
  addc %input
  st %acc
%end

%macro add8mem %acc, %input
  ld %acc
  add %input
  st %acc
%end

%macro sub64mem %acc, %input
  ld %acc + 7
  sub %input + 7
  st %acc + 7

  ld %acc + 6
  subc %input + 6
  st %acc + 6

  ld %acc + 5
  subc %input + 5
  st %acc + 5

  ld %acc + 4
  subc %input + 4
  st %acc + 4

  ld %acc + 3
  subc %input + 3
  st %acc + 3

  ld %acc + 2
  subc %input + 2
  st %acc + 2

  ld %acc + 1
  subc %input + 1
  st %acc + 1

  ld %acc
  subc %input
  st %acc
%end

%macro sub32mem %acc, %input
  ld %acc + 3
  sub %input + 3
  st %acc + 3

  ld %acc + 2
  subc %input + 2
  st %acc + 2

  ld %acc + 1
  subc %input + 1
  st %acc + 1

  ld %acc
  subc %input
  st %acc
%end

%macro sub16mem %acc, %input
  ld %acc + 1
  sub %input + 1
  st %acc + 1

  ld %acc
  subc %input
  st %acc
%end

%macro sub8mem %acc, %input
  ld %acc
  sub %input
  st %acc
%end

%macro add6stack %acc, %input
  ld %acc
  add %input
  st %acc

  ld %acc + 1
  addc %input + 1
  st %acc + 1

  ld %acc + 2
  addc %input + 2
  st %acc + 2

  ld %acc + 3
  addc %input + 3
  st %acc + 3

  ld %acc + 4
  addc %input + 4
  st %acc + 4

  ld %acc + 5
  addc %input + 5
  st %acc + 5

  ld %acc + 6
  addc %input + 6
  st %acc + 6

  ld %acc + 7
  addc %input + 7
  st %acc + 7
%end
