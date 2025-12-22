# Practice Test Coverage Analysis

## Functions in practice.lisp

### ✅ Well Tested Functions

1. **practice-command?** - ✅ Fully tested
   - Full command parsing (`/practice c lightb`)
   - Partial prefixes (`/p`, `/pr`, `/prac`)
   - Bare commands (`/p`, `/practice`)
   - Non-matching commands
   - Edge cases (non-slash, different slash commands)

2. **practice-matches-any-pattern?** - ✅ Fully tested
   - First pattern match
   - Second pattern match (partial)
   - No match
   - Empty pattern list

3. **practice-extract-mana** - ✅ Fully tested
   - 100% mana extraction
   - Various percentages (28%, 5%)
   - Full prompt format
   - No mana pattern found

4. **practice-start** - ✅ Well tested
   - Normal start
   - Already practicing (doesn't change command)
   - State variables set correctly
   - Divider mode set

5. **practice-stop** - ✅ Well tested
   - Normal stop
   - State cleared
   - Divider modes removed

6. **practice-enter-sleep** - ✅ Well tested
   - Sets sleep mode
   - Creates timer
   - Sends sleep command
   - Sets divider mode

7. **practice-exit-sleep** - ✅ Well tested
   - Clears sleep mode
   - Cancels timer
   - Sends stand and resume commands
   - Removes divider mode

8. **practice-telnet-hook** - ✅ Partially tested
   - ✅ Failure pattern detection ("You failed.", "You lost your concentration.")
   - ✅ Mana exhaustion detection ("You don't have enough mana.")
   - ✅ Mana restoration (100% mana)
   - ✅ Partial mana (stays in sleep)
   - ✅ Inactive when not in practice mode

9. **practice-user-input-hook** - ✅ Well tested
   - `/p <command>` starts practice
   - `/p stop` stops practice
   - Non-matching commands don't handle
   - Sets _user-input-handled_ correctly

### ❌ Missing Test Coverage

1. **practice-add-retry-pattern** - ❌ NOT TESTED
   - Adding new pattern
   - Pattern already exists (should echo message)
   - Pattern added to list

2. **practice-send-empty** - ❌ NOT TESTED
   - Timer callback sends empty string when in sleep mode
   - Does nothing when not in sleep mode
   - Does nothing when not in practice mode

3. **practice-quit-on-hunger-thirst** - ❌ NOT TESTED
   - Detects hunger/thirst damage
   - Calls practice-stop
   - Sends "quit" command
   - This is a critical safety feature!

4. **practice-telnet-hook - Missing Scenarios:**
   - ❌ Hunger/thirst damage detection (line 198-199)
   - ❌ Mana threshold detection (< 20% mana, line 205-208)
   - ❌ "You are already" retry pattern (third pattern in list)
   - ❌ Multiple retry patterns in same message
   - ❌ Enter sleep when already in sleep mode (should be idempotent)
   - ❌ Exit sleep when not in sleep mode (should be idempotent)

5. **practice-user-input-hook - Missing Scenarios:**
   - ❌ `/p` with no args when not practicing (shows "Not practicing" message)
   - ❌ `/p` with no args when practicing (shows status with command)
   - ❌ `/p` with no args when practicing and sleeping (shows "(sleeping)")

6. **practice-stop - Missing Edge Cases:**
   - ❌ Stop when not practicing (should echo "Not currently practicing")
   - ❌ Stop with active timer (timer cancellation tested indirectly)

## Critical Missing Tests

### High Priority (Safety Features)

1. **Hunger/thirst damage detection** - This is a safety feature that quits the game!
   - Test: `practice-telnet-hook` with "Your hunger grazes you"
   - Test: `practice-telnet-hook` with "Your thirst grazes you"
   - Test: `practice-quit-on-hunger-thirst` function directly

### Medium Priority (Core Functionality)

2. **Mana threshold detection** - Enters sleep when mana < 20%
   - Test: `practice-telnet-hook` with prompt showing 15% mana
   - Test: `practice-telnet-hook` with prompt showing 19% mana
   - Test: `practice-telnet-hook` with prompt showing 20% mana (should not sleep)

3. **Status display** - `/p` with no args
   - Test: Shows status when practicing
   - Test: Shows status when practicing and sleeping
   - Test: Shows "Not practicing" when not active

4. **practice-add-retry-pattern** - Configuration function
   - Test: Adding new pattern
   - Test: Pattern already exists

### Low Priority (Edge Cases)

5. **practice-send-empty** - Timer callback
   - Test: Sends empty string in sleep mode
   - Test: Does nothing when not in sleep mode

6. **Idempotency tests**
   - Test: Enter sleep when already in sleep
   - Test: Exit sleep when not in sleep
   - Test: Stop when not practicing

## Coverage Summary

- **Functions tested**: 8/11 (73%)
- **Critical paths tested**: Most core functionality
- **Safety features tested**: 0/1 (0%) - Hunger/thirst detection missing!
- **Edge cases tested**: Some, but missing several

## Recommendations

1. **Add hunger/thirst damage tests immediately** - This is a safety feature
2. **Add mana threshold tests** - Important for sleep mode logic
3. **Add status display tests** - User-facing feature
4. **Add practice-add-retry-pattern tests** - Configuration function
5. **Add practice-send-empty tests** - Timer callback functionality
