#!/usr/bin/env python3
import asyncio
from collections import Counter
import json
import logging
import struct
import sys
import time

import psutil

#SERVER = ("cron", 94733) TODO
SERVER = ("localhost", 5678)

EDITORS = [
    "emacs",
    "vim",
    "vi",
    "kate",
    "gedit",
    "subl",
    "ed",
    "atom",
]

UPDATE_INTERVAL = 1 # seconds TODO 30
RECONNECT_INTERVAL = 5 # seconds
RECONNECT_COUNT = 2 # tries TODO 12

LOG_FORMAT = "%(asctime)-15s %(message)s"


def get_counts():
    procs = ((p.name(), p.username()) for p in psutil.process_iter())
    editors = (p for p in procs if p[0] in EDITORS)
    unique = set(editors)
    return Counter(p[0] for p in unique)


class ConnectionError(Exception):
    pass


async def connect(server):
    for _ in range(RECONNECT_COUNT):
        try:
            _, writer = await asyncio.open_connection(server[0], server[1])
        except OSError as err:
            logging.warning(
                "Failed to connect (%s), retrying in %d seconds",
                err.strerror,
                RECONNECT_INTERVAL
            )
            await asyncio.sleep(RECONNECT_INTERVAL)
        else:
            return writer

    logging.error("Retry count exceeded, shutting down")
    raise ConnectionError()


async def send_data(server):
    loop = asyncio.get_event_loop()

    try:
        writer = await connect(server)
    except ConnectionError:
        loop.stop()
        return

    while True:
        writer.write(json.dumps(get_counts()).encode())
        await asyncio.sleep(UPDATE_INTERVAL)


def main():
    logging.basicConfig(level=logging.INFO, format=LOG_FORMAT)
    logging.info("Starting katecount per-host daemon")

    if sys.platform == 'win32':
        loop = asyncio.ProactorEventLoop()
    else:
        loop = asyncio.SelectorEventLoop()

    asyncio.set_event_loop(loop)

    loop.create_task(send_data(SERVER))

    try:
        loop.run_forever()
    except KeyboardInterrupt:
        loop.stop()
    finally:
        loop.run_until_complete(loop.shutdown_asyncgens())
        loop.close()


if __name__ == '__main__':
    sys.exit(main())
