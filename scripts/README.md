# Zamlbie Testing Scripts

This directory contains testing utilities for the Zamlbie game.

## Scalability Test

`scalability_test.py` is a tool for testing how well the Zamlbie server handles multiple concurrent clients. It spawns a specified number of client instances that join an existing game and simulate player activity by sending random directional inputs.

### Setting up Python Environment

```bash
# Create a virtual environment
python3 -m venv zamlbie_env

# Activate the virtual environment
# On macOS/Linux with Bash:
source zamlbie_env/bin/activate
# On macOS/Linux with Fish:
source zamlbie_env/bin/activate.fish
# On Windows:
# zamlbie_env\Scripts\activate

# Install required dependencies using requirements.txt
pip3 install -r requirements.txt

# Or install dependencies manually
pip3 install pynput
```

### Prerequisites

- Python 3.6 or higher
- Dependencies listed in `requirements.txt`:
  ```bash
  pip3 install -r requirements.txt
  ```

### macOS Accessibility Permissions

If you see this error: "This process is not trusted! Input event monitoring will not be possible until it is added to accessibility clients."

1. Go to System Preferences > Security & Privacy > Privacy > Accessibility
2. Click the lock icon to make changes (you'll need to enter your password)
3. Add Terminal (or iTerm/your terminal application) to the list of allowed apps
4. If using a virtual environment with a specific Python interpreter, you may need to add that interpreter as well
5. Restart your terminal application

For Python running within an IDE (like VS Code or PyCharm), you may need to add the IDE to the accessibility list instead.

### Usage

1. First, start your Zamlbie server:
   ```bash
   dune exec server
   ```

2. Create a new game using the client:
   ```bash
   dune exec client -- --create --max-player-count <number>
   ```
   Note the game ID that is generated.

3. Run the scalability test script:
   ```bash
   python3 scalability_test.py --n <number_of_clients> --game-id <game_id>
   ```
   
   Additional options:
   - `--client-path`: Path to client executable (default: `./_build/default/bin/main_client.exe`)

### Examples

```bash
# Spawn 10 clients and join them to game ID "abc123"
python3 scalability_test.py --n 10 --game-id abc123

# Use a custom client executable
python3 scalability_test.py --n 5 --game-id xyz789 --client-path ./custom_client.exe

# Create a large game for 200 players and test it
dune exec client -- --create --max-player-count 200 --width 100 --height 100 --tick-delta 0.1 --walls-per-floor 50 --staircases-per-floor 10 --number-of-floor 5 --time-limit 300
python3 scalability_test.py --n 199 --game-id <game_id>
```

### Operation

- Each spawned client will perform random movements (up, down, left, right) at random intervals between 50ms and 1 second
- Press 'q' at any time to quit the test and terminate all client processes
- The script will handle cleanup of all spawned processes on exit

### Troubleshooting

If you encounter issues:

1. Ensure the server is running and accessible
2. Verify that the game ID exists and has enough space for new players
3. Check that the client executable path is correct
4. Make sure port requirements for the Zamlbie server are satisfied
5. If you receive "Command not found" errors, ensure you're using `python3` instead of `python`
6. For permission issues, make the script executable: `chmod +x scalability_test.py`
7. For Fish shell users, if you get errors with virtual environment activation, try:
   ```fish
   source zamlbie_env/bin/activate.fish
   ```
8. If you see accessibility trust errors on macOS, follow the macOS Accessibility Permissions section above

### Note

This script is intended for testing purposes only. Spawning too many clients may affect your system performance.
