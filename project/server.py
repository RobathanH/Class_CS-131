import os, sys, time, re
import asyncio
import aiohttp
import async_timeout
import json

# Server info and communication graph
ports = {
	'Goloman': 11710,
	'Hands': 11711,
	'Holiday': 11712,
	'Welsh': 11713,
	'Wilkes': 11714
}

communication_links = {
	'Goloman': ['Hands', 'Holiday', 'Wilkes'],
	'Hands': ['Goloman', 'Wilkes'],
	'Holiday': ['Goloman', 'Welsh', 'Wilkes'],
	'Welsh': ['Holiday'],
	'Wilkes': ['Goloman', 'Hands', 'Holiday']
}


# Runtime vars
client_info = {} # format -> client_id: [latitude, longitude, time_sent, AT_message]

# constants
API_KEY = #DELETED MINE FOR SECURITY PURPOSES - PLEASE REPLACE
MAKE_REQUESTS = True # If True, server will send requests to Google Places API. If False, it will instead use a dummy JSON response
DUMMY_RESPONSE = "DUMMY JSON RESPONSE FROM GOOGLE"



# main
def main():
	# Reading input and setting up
	if len(sys.argv) != 2:
		print('Incorrect command format: python3 server.py server_id\n')
		sys.exit(1)
	
	global server_id
	server_id = sys.argv[1]
	if server_id not in ports.keys():
		print('Invalid server ID: Must be in ' + str(ports.keys()))
		sys.exit(1)

	global log_file
	log_file = open(server_id + '_log.txt', 'a+')

	
	# Creating event loop
	global loop
	loop = asyncio.get_event_loop()
	
	
	# time to set up the server
	serverSetup_coroutine = asyncio.start_server(acceptConnection, '127.0.0.1', ports[server_id], loop = loop)
	server = loop.run_until_complete(serverSetup_coroutine)

	
	# just loop the server until a keyboard interrupt
	print('Server started: Running on ' + str(server.sockets[0].getsockname()))
	try:
		loop.run_forever()
	except KeyboardInterrupt:
		print('Shutting Down.')
		log_file.close()
		server.close()
		loop.run_until_complete(server.wait_closed())
		loop.close()

	
	




#----------------- I/O Helper Functions ----------------------

# sends message through given StreamWriter
# writer - StreamWriter object
# msg - string (not bytes)
# returns a boolean, true if write succeeded
async def sendMsg(writer, msg):

	if msg == None:
		return False

	try:
		writer.write(msg.encode())
		await writer.drain()
	except:
		print('Error sending data to ' + writer.get_extra_info('peername'))
		print('Message: ' + msg)
		return False

	return True

# writes message into log_file
# msg - string
# returns a boolean, true if write succeeded
async def logMsg(msg):
	if msg == None:
		return False

	try:
		log_file.write(msg)
		log_file.flush()
	except:
		print('Error writing to log file.')
		print('Message: ' + msg)
		return False

	return True



#---------------- I/O Handlers --------------------

# Sends message to all connected servers, except those on the given blacklist
# msg: string of characters, with NO newline - newline will be added automatically
# blacklist: list of server ID's which shouldn't be sent to: originator of AT message and whichever server sent the AT message to this server
# doesn't return anything
async def sendToServers(msg, blacklist):
	# log start of propogation
	if len(blacklist) > 0:
		await logMsg("Propogating Information. Blacklist: " + ', '.join(blacklist) + "\n")
	else:
		await logMsg("Propogating Information to all connected servers.\n")

	for target in communication_links[server_id]:
		if target not in blacklist:
			try:
				# open connection (and log)
				await logMsg("Opening connection with " + target + ": 127.0.0.1:" + str(ports[target]) + "\n")
				read, write = await asyncio.open_connection('127.0.0.1', ports[target], loop = loop)
				
				# send message (and log)
				await logMsg("Sending message: " + msg + "\n")
				await sendMsg(write, msg + "\n")
				
				# close connection (and log)
				await logMsg("Closing connection.\n")
				write.close()
			
			except:
				# log error
				await logMsg("Error connecting! Moving on.\n")


# entry point for all incoming connections, both client and server
# largely functions as a scheduler for handleConnection
def acceptConnection(reader, writer):
	asyncio.ensure_future(handleConnection(reader, writer))

async def handleConnection(reader, writer):

	host, port = writer.get_extra_info('peername')
	
	# read input line by line until done
	while not reader.at_eof():

		try:
			rawData = await reader.readline()
		except:
			return

		rawData = rawData.decode() # raw data contains the message as a string
		data = rawData.split(' ')
		data = list(filter(lambda x: len(x) > 0, data))

		# remove trailing newline
		if len(data) != 0:
			if data[-1] == '\n':
				data.pop()
			elif data[-1][-1] == '\n':
				data[-1] = data[-1][:-1]

		# get time received
		timeReceived = time.time()


		if len(data) != 0:
			# log incoming message
			await logMsg("Received message from " + host + ":" + str(port) + " --- " \
					+ "Message: " + ' '.join(data) + "\n")

			# process the command, then wait for the next command
			response = await parseCommand(data, timeReceived)

			if response != None:
				await logMsg("Sending message to " + host + ":" + str(port) + " --- " \
						+ "Message: " + response + "\n")
				await sendMsg(writer, response)

	
	# close connection
	writer.close()
	
	

# -------------------- HTTP helpers ------------------------
# sends asynchronous http request to google API
# inputs:
# longitude, latitude - strings with format: +24565.342 or -214241.1532, etc
# radius - string with format: 1,2,3,...,50
# maxResults - int in [0,20]
# returns JSON response as string
async def getPlaces(lat, lon, rad, maxResults):
	url = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=%s,%s&radius=%d&key=%s' % (lat, lon, rad, API_KEY)
	
	async with aiohttp.ClientSession() as sess:
		async with async_timeout.timeout(10):
			async with sess.get(url) as response:
				answer = await response.json()

	# limit number of responses
	answer['results'] = answer['results'][:maxResults]
	return re.sub(r'\n+', '\n', json.dumps(answer, indent=3))



# -------------------- Command Handlers ---------------------------------

# Respond to input command - uses commandValid to check that command is valid
# msg - input command string, split on whitespace into a list, with NO newlines
# returns None, or the message to send back
async def parseCommand(msg, timeReceived):

	# check if command is valid
	valid = await commandValid(msg)

	# if message is invalid, send invalid message response
	if not valid:
		return '? ' + ' '.join(msg)
		

	if msg[0] == 'IAMAT':
		ID = msg[1]
		pos = msg[2]
		timeSent = float(msg[3])
		
		# find time difference
		timeDif = timeReceived - timeSent
		if timeDif < 0:
			timeDif = '-%f' %timeDif
		else:
			timeDif = '+%f' %timeDif
		
		pos = convertPosString(pos)

		# if existing records of this client are more outdated than this message
		# (I don't know when they wouldn't be, but it's good to check)
		if not (ID in client_info.keys() and client_info[ID][2] >= timeSent):

			# create AT command for response
			response = "AT %s %s %s %s%s %s" % (server_id, timeDif, ID, pos[0], pos[1], str(timeSent))

			client_info[ID] = [pos[0], pos[1], timeSent, response]

			# propogate new info to other servers
			asyncio.ensure_future(sendToServers(response, []))

			# return response to be sent back to client
			return response
		

	elif msg[0] == 'WHATSAT':
		ID = msg[1]
		rad = int(msg[2])
		maxResults = int(msg[3])

		lat, lon, timeSent, origMessage = client_info[ID]

		if MAKE_REQUESTS:
			response = await getPlaces(lat, lon, rad, maxResults)
			return origMessage + "\n" + response + "\n"
		else:
			return origMessage + "\n" + DUMMY_RESPONSE + "\n"

	elif msg[0] == 'AT':
		originator = msg[1]
		ID = msg[3]
		pos = msg[4]
		timeSent = float(msg[5])

		pos = convertPosString(pos)

		# if existing records of this client are more outdated than this message
		if not (ID in client_info.keys() and client_info[ID][2] >= timeSent):

			client_info[ID] = [pos[0], pos[1], timeSent, ' '.join(msg)]
		
			# propogate msg to other servers
			asyncio.ensure_future(sendToServers(' '.join(msg), [originator]))
				
		

	# default return val - if no response to command is necessary
	return None

# Regexes for validating fields
posPattern = re.compile(r'^[+-][0-9]+.[0-9]+[+-][0-9]+.[0-9]+$')
timePattern = re.compile(r'^[0-9]*.[0-9]+$|^[0-9]+.[0-9]*$')
timeDifPattern = re.compile(r'^[+-][0-9]+.[0-9]+$')
intPattern = re.compile(r'^[0-9]+$')

# helper function - splits latitude/longitude string into length-2 list of float-formatted strings
def convertPosString(pos):
	# convert lat/lon to floats
	posList = re.split(r'([+-])', pos)
	newPos = []
	for i in range(len(posList) - 1):
		if posList[i] in ['+', '-']:
			newPos.append(posList[i] + posList[i + 1])
	return newPos


# return true if command is a valid command
# msg - input command string, split on whitespace
# fromClient is a boolean, self-explanatory
async def commandValid(msg):
	if len(msg) == 0:
		return False

	if msg[0] == 'IAMAT':
		if len(msg) != 4:
			return False
		
		pos = msg[2]
		timeSent = msg[3]
		
		# check using regular expression
		if not (posPattern.match(pos) and timePattern.match(timeSent)):
			return False

	elif msg[0] == 'WHATSAT':
		if len(msg) != 4:
			return False
		
		ID = msg[1]
		rad = msg[2]
		maxItems = msg[3]
		
		# check regex
		if not (intPattern.match(rad) and intPattern.match(maxItems)):
			return False
		
		# check that rad and maxItems are in bounds
		if not (int(rad) <= 50 and int(maxItems) <= 20):
			return False
		
		# check that ID is in client list
		if ID not in client_info.keys():
			return False
		
	elif msg[0] == 'AT':
		if len(msg) != 6:
			return False

		originator = msg[1]
		timeDif = msg[2]
		pos = msg[4]
		timeSent = msg[5]

		# check regex
		if not (originator in ports.keys() and timeDifPattern.match(timeDif) and posPattern.match(pos) and timePattern.match(timeSent)):
			return False
	
	# command unrecognized
	else:
		return False

	# if tests are all passed
	return True



if __name__ == '__main__':
	main()
