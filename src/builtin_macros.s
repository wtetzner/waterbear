; Safe long-range forms of the VMU conditional branch instructions.
; Jump optimization may later fold these expansions back to direct branches.

%macro bzo %target
  bnz .next%
  jmpf %target
.next%:
%end

%macro bnzo %target
  bz .next%
  jmpf %target
.next%:
%end

%macro bpo %direct, %bit, %target
  bp %direct, %bit, .true%
  br .next%
.true%:
  jmpf %target
.next%:
%end

%macro bpco %direct, %bit, %target
  bpc %direct, %bit, .true%
  br .next%
.true%:
  jmpf %target
.next%:
%end

%macro bno %direct, %bit, %target
  bn %direct, %bit, .true%
  br .next%
.true%:
  jmpf %target
.next%:
%end

%macro dbnzo %value, %target
  dbnz %value, .true%
  br .next%
.true%:
  jmpf %target
.next%:
%end

%macro beo %value, %target
  bne %value, .next%
  jmpf %target
.next%:
%end

%macro beo %source, %value, %target
  bne %source, %value, .next%
  jmpf %target
.next%:
%end

%macro bneo %value, %target
  be %value, .next%
  jmpf %target
.next%:
%end

%macro bneo %source, %value, %target
  be %source, %value, .next%
  jmpf %target
.next%:
%end
