module C = Configurator.V1

let unix = {|
#include <math.h>
void *addr = &sin;
int main(void) {
    return 0;
}
|}

let windows = {|
#include <winsock2.h>
void *addr = &gethostname;
int main(void) {
    return 0;
}
|}

let main c =
  let code = if Sys.os_type = "Win32" then windows else unix in
  let b = C.c_test c code in
  let f = open_out_bin "out" in
  output_char f (if b then '1' else '0');
  close_out f

let () = C.main ~name:"configurator-c-libraries" main
