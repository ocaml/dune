The issue with the oneshot webserver used for testing
package management, where attempting to send a large file causes the
connection to be reset before the transfer completes, is now fixed.

Make a large file:
  $ dd if=/dev/zero of=./large-file iflag=fullblock,count_bytes status=none count=64M

Run the oneshot webserver to serve the large file:
  $ webserver_oneshot --content-file ./large-file --port-file port.txt &
  $ until test -f port.txt ; do sleep 0.1; done

Download the large file with curl:
  $ curl -sS -o large-file-copy http://localhost:$(cat port.txt) > /dev/null
