#!/usr/bin/env python3

import subprocess
import threading
import random
import time
import argparse
import os
import signal
import sys
from pynput import keyboard

# Parse command-line arguments
parser = argparse.ArgumentParser(
    description='Test scalability of Zamlbie by spawning multiple clients')
parser.add_argument('--n', type=int, default=5,
                    help='Number of client instances to spawn')
parser.add_argument('--game-id', type=str, required=True,
                    help='Game ID to join')
parser.add_argument('--client-path', type=str, default='./_build/default/bin/main_client.exe',
                    help='Path to the client executable')
args = parser.parse_args()

# Global variables
running = True
clients = []
client_threads = []

# Define the possible arrow key actions
ARROW_KEYS = ['Up', 'Down', 'Left', 'Right']


def send_random_action(client_proc):
    """Send a random arrow key action to the client process"""
    if not client_proc.poll():
        action = random.choice(ARROW_KEYS)
        try:
            if os.name == 'nt':  # Windows
                # Windows implementation would vary based on how your client accepts input
                pass
            else:  # Unix-based (macOS, Linux)
                # Send arrow key via ANSI escape sequence
                if action == 'Up':
                    client_proc.stdin.write('\x1b[A'.encode())
                elif action == 'Down':
                    client_proc.stdin.write('\x1b[B'.encode())
                elif action == 'Right':
                    client_proc.stdin.write('\x1b[C'.encode())
                elif action == 'Left':
                    client_proc.stdin.write('\x1b[D'.encode())
                client_proc.stdin.flush()
        except:
            pass


def client_action_thread(client_proc, client_id):
    """Thread that sends random actions to a client at random intervals"""
    global running
    while running and not client_proc.poll():
        try:
            send_random_action(client_proc)
            # Random interval between 50ms and 1s
            time.sleep(random.uniform(0.05, 1.0))
        except:
            break
    print(f"Client {client_id} action thread exited")


def spawn_client(client_id):
    """Spawn a new client process and start its action thread"""
    try:
        client_proc = subprocess.Popen(
            [args.client_path, '--join', args.game_id],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE
        )
        print(f"Started client {client_id}")

        # Start a thread to send random actions to this client
        action_thread = threading.Thread(
            target=client_action_thread,
            args=(client_proc, client_id),
            daemon=True
        )
        action_thread.start()

        return client_proc, action_thread
    except Exception as e:
        print(f"Failed to spawn client {client_id}: {e}")
        return None, None


def on_press(key):
    """Handle keyboard input to quit the script"""
    global running
    try:
        if key.char == 'q':
            print("Quitting...")
            running = False
            return False  # Stop listener
    except:
        pass


def main():
    """Main function to run the scalability test"""
    global running, clients, client_threads

    print(
        f"Starting scalability test with {args.n} clients joining game {args.game_id}")
    print("Press 'q' to quit")

    # Start keyboard listener for quit command
    listener = keyboard.Listener(on_press=on_press)
    listener.start()

    # Spawn clients
    for i in range(args.n):
        client_proc, thread = spawn_client(i)
        if client_proc:
            clients.append(client_proc)
            client_threads.append(thread)
        time.sleep(0.5)  # Short delay between spawning clients

    # Wait until quit is requested
    while running:
        try:
            time.sleep(0.5)
        except KeyboardInterrupt:
            running = False
            break

    # Clean up
    print("Terminating clients...")
    for client in clients:
        if client and not client.poll():
            try:
                # Try graceful termination first
                if os.name == 'nt':  # Windows
                    client.terminate()
                else:  # Unix
                    client.send_signal(signal.SIGTERM)

                # Wait for termination
                client.wait(timeout=1.0)
            except:
                # Force kill if termination fails
                try:
                    client.kill()
                except:
                    pass

    print("Scalability test completed")


if __name__ == "__main__":
    main()
