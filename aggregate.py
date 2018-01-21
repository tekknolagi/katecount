#!/usr/bin/env python3
import asyncio #TODO
import os
import string
import struct #TODO
import subprocess
import sys

def ascii_range(start, end):
    return (chr(x) for x in range(ord(start), ord(end)))

HOSTS = (
    ["lab116" + n for n in ascii_range('a', 'x')] +
    ["lab118" + n for n in ascii_range('a', 'x')] +
    ["lab120" + n for n in ascii_range('a', 'v')] +
    ["vm-hw0" + n for n in ascii_range('0', '9')]
)

CLIENT_DIR = "katecount"
CLIENT_SCRIPT = os.path.join(CLIENT_DIR, "per_host.py")
CLIENT_LOG_DIR = os.path.join(CLIENT_DIR, "logs")


class LenPrefParser(asyncio.Protocol):
    LP = struct.Struct('!I')

    def __init__(self):
        super().__init__()
        self.buffer = bytearray()
        self.next_len = None

    def data_received(self, data):
        self.buffer.extend(data)

        while True:
            if self.next_len is None and len(self.buffer) >= self.LP.size:
                self.next_len = self.LP.unpack(self.buffer[:self.LP.size])[0]
                del self.buffer[:self.LP.size]
            elif self.next_len is not None and len(self.buffer) >= self.next_len:
                msg = bytes(self.buffer[:self.next_len])
                del self.buffer[:self.next_len]
                self.next_len = None
                self.msg_received(msg)
            else:
                break


def spawn_on(hostname):
    subprocess.run([
        "ssh",
        hostname,
        "nohup {} >>{} 2>&1".format(
            CLIENT_SCRIPT,
            os.path.join(CLIENT_LOG_DIR, "{}.log".format(hostname))
        )
    ])


def main():
    print(HOSTS)


if __name__ == '__main__':
    sys.exit(main())
