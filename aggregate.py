#!/usr/bin/env python3
import asyncio
import json
import os
from pathlib import Path
import socket
import string
import struct
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
STATS_PATH = os.path.join(Path.home(), "public_html", "editors.json")
UPDATE_INTERVAL = 5
LISTEN_PORT = 94733


class Host:
    def __init__(self, hostname):
        self.info = socket.getaddrinfo(hostname, None)


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


class ClientLink(LenPrefParser):
    def __init__(self, hosts, stats):
        super().__init__()
        self.transport = None
        self.hosts = hosts
        self.stats = stats

    def connection_made(self, transport):
        self.transport = transport

    def msg_received(self, data):
        try:
            counts = json.loads(data)
        except json.JSONDecodeError:
            # TODO: self.name
            logger.exception("Got bad JSON from client %s", self.name)


def spawn_on(hostname):
    subprocess.run([
        "ssh",
        hostname,
        "nohup {} >>{} 2>&1".format(
            CLIENT_SCRIPT,
            os.path.join(CLIENT_LOG_DIR, "{}.log".format(hostname))
        )
    ])


async def write_stats_loop(path, stats):
    while True:
        with open(path, 'w') as f:
            json.dump(stats, f)
        await asyncio.sleep(UPDATE_INTERVAL)


def main():
    logging.basicConfig(level=logging.INFO, format=LOG_FORMAT)
    logging.info("Starting katecount aggregator")

    if sys.platform == 'win32':
        loop = asyncio.ProactorEventLoop()
    else:
        loop = asyncio.SelectorEventLoop()

    asyncio.set_event_loop(loop)

    stats = {}
    hosts = {name: Host(name) for name in HOSTS}

    server = loop.create_server(lambda: ClientLink(hosts, stats), None, LISTEN_PORT);
    update = loop.create_task(write_stats_loop(STATS_PATH, stats))

    loop.create_task(server)
    loop.create_task(update)

    try:
        loop.run_forever()
    except KeyboardInterrupt:
        loop.stop()
    finally:
        loop.run_until_complete(loop.shutdown_asyncgens())
        loop.close()


if __name__ == '__main__':
    sys.exit(main())
