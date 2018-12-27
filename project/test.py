import socket
import sys
import time

DEFAULT_LOC = "+53.2734-7.77832031"
DEFAULT_RAD = "10"
DEFAULT_MAX_ITEMS = "5"

ports = {
	'Goloman': 11710,
	'Hands': 11711,
	'Holiday': 11712,
	'Welsh': 11713,
	'Wilkes': 11714
}


if len(sys.argv) != 4:
	print("Incorrect format: python3 test.py <server> <ID> <Command>")
	sys.exit(1)

if sys.argv[3] not in ['IAMAT', 'WHATSAT']:
	print("Invalid command.")
	sys.exit(1)

if sys.argv[1] not in ports.keys():
	print("Invalid server.")
	sys.exit(1)




s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect(("127.0.0.1", ports[sys.argv[1]]))

if sys.argv[3] == 'IAMAT':
	msg = "IAMAT " + sys.argv[2] + " " + DEFAULT_LOC + " " + str(time.time()) + "\n"

if sys.argv[3] == 'WHATSAT':
	msg = "WHATSAT " + sys.argv[2] + " " + DEFAULT_RAD + " " + DEFAULT_MAX_ITEMS + "\n"
	
s.send(msg.encode())

print(s.recv(1024).decode())
s.close()
