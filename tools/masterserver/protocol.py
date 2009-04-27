#!/usr/bin/env python
#
# This file is part of the Warzone 2100 Resurrection Project.
# Copyright (c) 2007-2009  Warzone 2100 Resurrection Project
#
# Warzone 2100 is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# You should have received a copy of the GNU General Public License
# along with Warzone 2100; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
#
################################################################################
# import from __future__
from __future__ import with_statement

#
################################################################################
# Get the things we need.
import struct
from pkg_resources import parse_version
from game import *

__all__ = ['Protocol', 'BinaryProtocol']

def _encodeCString(string, buf_len):
	# If we haven't got a string return an empty one
	if not string:
		return str('\0' * buf_len)

	# Make sure the string fits
	string = string[:buf_len - 1]

	# Add a terminating NUL and fill the rest of the buffer with NUL bytes
	string = string.ljust(buf_len, "\0")

	# Make sure *not* to use a unicode string
	return str(string)

class Protocol(object):
	# Size of a single game is undefined at compile time
	size = None

	def encodeSingle(data):
		pass

	def encodeMultiple(data):
		pass

	def decodeSingle(data, game = Game(None)):
		pass

	def decodeMultiple(data):
		pass

class BinaryProtocol(Protocol):
	# Binary struct format to use (GAMESTRUCT)
	gameFormat = {}

	# >= 2.0 data
	name_length          = 64
	host_length          = 16

	gameFormat['2.0'] = struct.Struct('!%dsII%ds6I' % (name_length, host_length))

	# >= 2.2 data
	misc_length          = 64
	extra_length         = 255
	versionstring_length = 64
	modlist_length       = 255

	gameFormat['2.2'] = struct.Struct(gameFormat['2.0'].format + '%ds%ds%ds%ds10I' % (misc_length, extra_length, versionstring_length, modlist_length))

	countFormat = struct.Struct('!I')

	size = property(fget = lambda self: self.gameFormat.size)

	def __init__(self, version = '2.2'):
		# Size of a single game = sizeof(GAMESTRUCT)
		if   parse_version('2.0') <= parse_version(version) < parse_version('2.2'):
			self.gameFormat = BinaryProtocol.gameFormat['2.0']
		elif parse_version('2.2') <= parse_version(version):
			self.gameFormat = BinaryProtocol.gameFormat['2.2']

		self.version = version

	def _encodeName(self, game):
		return _encodeCString(game.description, self.name_length)

	def _encodeHost(self, game):
		return _encodeCString(game.host, self.host_length)

	def _encodeMisc(self, game):
		return _encodeCString(game.misc, self.misc_length)

	def _encodeExtra(self, game):
		return _encodeCString(game.extra, self.extra_length)

	def _encodeVersionString(self, game):
		return _encodeCString(game.multiplayerVersion, self.versionstring_length)

	def encodeSingle(self, game):
		if   parse_version('2.0') <= parse_version(self.version) < parse_version('2.2'):
			return self.gameFormat.pack(
				self._encodeName(game),
				game.size or self.size, game.flags,
				self._encodeHost(game),
				game.maxPlayers, game.currentPlayers, game.user1, game.user2, game.user3, game.user4)
		elif parse_version('2.2') <= parse_version(self.version):
			return self.gameFormat.pack(
				self._encodeName(game),
				game.size or self.size, game.flags,
				self._encodeHost(game),
				game.maxPlayers, game.currentPlayers, game.user1, game.user2, game.user3, game.user4,
				self._encodeMisc(game),
				self._encodeExtra(game),
				self._encodeVersionString(game),
				game.modlist,
				game.lobbyVersion, game.game_version_major, game.game_version_minor, game.private,
				game.pure, game.Mods, game.future1, game.future2, game.future3, game.future4)

	def encodeMultiple(self, games):
		message = self.countFormat.pack(len(games))
		for game in games:
			message += self.encodeSingle(game)
		return message

	def decodeSingle(self, data, game = Game(None)):
		decData = {}

		if   parse_version('2.0') <= parse_version(self.version) < parse_version('2.2'):
			(decData['name'], game.size, game.flags, decData['host'], game.maxPlayers, game.currentPlayers,
				game.user1, game.user2, game.user3, game.user4) = self.gameFormat.unpack(data)
		elif parse_version('2.2') <= parse_version(self.version):
			(decData['name'], game.size, game.flags, decData['host'], game.maxPlayers, game.currentPlayers,
				game.user1, game.user2, game.user3, game.user4,
				game.misc, game.extra, decData['multiplayer-version'], game.modlist, game.lobbyVersion,
				game.game_version_major, game.game_version_minor, game.private, game.pure, game.Mods, game.future1,
				game.future2, game.future3, game.future4) = self.gameFormat.unpack(data)

		for strKey in ['name', 'host', 'multiplayer-version']:
			decData[strKey] = decData[strKey].strip("\0")

		game.misc = game.misc.strip("\0")
		game.extra = game.extra.strip("\0")
		game.modlist = game.modlist.strip("\0")

		game.data.update(decData)
		return game

	def decodeMultiple(self, data):
		countMsg = data[:self.countFormat.size]
		data = data[self.countFormat.size:]
		count = self.countFormat.unpack(countMsg)
		return [self.decodeSingle(data[size * i: size * i + size]) for i in range(count)]
