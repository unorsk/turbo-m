⏺ Code Review: Turbo-M Spaced Repetition System

  Executive Summary

  Your codebase is functional but has several architectural issues that limit extensibility, reusability, and maintainability. The main problems are: tight
  coupling between layers, lack of abstraction, hardcoded business rules, and missing configuration. Below is a detailed analysis with actionable
  recommendations.

  ---
  1. Separation of Concerns

  Issues:

  main.zig (lines 36-63): File loading logic mixed with CLI concerns
  - Parsing file format is in main instead of a dedicated module
  - Hardcoded delimiter (##) with no abstraction

  root.zig: Multiple responsibilities in one file
  - Core SRS algorithm (checkAnswer, normalizeString)
  - Training session state management (TrainingSession)
  - Utility functions (shuffle)

  tui.zig: UI logic tightly coupled with business logic
  - Model struct contains both UI state and session management
  - Event handlers directly manipulate session state

  Recommendations:

  // Proposed structure:
  src/
  ├── main.zig                    // Entry point only
  ├── cli/
  │   ├── args.zig               // Argument parsing
  │   └── commands.zig           // Command handlers
  ├── core/
  │   ├── session.zig            // TrainingSession
  │   ├── algorithm.zig          // SRS algorithms
  │   └── matcher.zig            // Answer matching logic
  ├── io/
  │   ├── loader.zig             // Deck loading interface
  │   └── formats/
  │       ├── simple.zig         // Current ## format
  │       └── anki.zig           // Future: Anki format
  ├── ui/
  │   └── tui.zig                // TUI implementation
  └── types.zig                   // Shared types

  ---
  2. Reusability Issues

  Problem Areas:

  main.zig:36-63: loadItemsFromFile is hardcoded
  - Can only load one format
  - No way to inject different parsers
  - Parsing logic not reusable

  root.zig:40-68: checkAnswer has hardcoded thresholds
  const threshold = if (normalized_expected.len > 3) @as(usize, 2) else @as(usize, 1);

  root.zig:70-73: calculateNextInterval is a stub
  pub fn calculateNextInterval(correct_count: u8) u32 {
      _ = correct_count;  // Unused!
      return 1;
  }

  Recommendations:

  2.1 Create a DeckLoader interface pattern:

  // src/io/loader.zig
  pub const DeckLoader = struct {
      ptr: *anyopaque,
      vtable: *const VTable,

      pub const VTable = struct {
          load: *const fn (ptr: *anyopaque, alloc: std.mem.Allocator, path: []const u8) anyerror![]Item,
          deinit: *const fn (ptr: *anyopaque) void,
      };

      pub fn load(self: DeckLoader, alloc: std.mem.Allocator, path: []const u8) ![]Item {
          return self.vtable.load(self.ptr, alloc, path);
      }
  };

  // src/io/formats/simple.zig
  pub const SimpleDeckLoader = struct {
      delimiter: []const u8 = "##",

      pub fn loader(self: *SimpleDeckLoader) DeckLoader {
          return .{
              .ptr = self,
              .vtable = &.{
                  .load = load,
                  .deinit = deinit,
              },
          };
      }

      fn load(ptr: *anyopaque, alloc: std.mem.Allocator, path: []const u8) ![]Item {
          const self: *SimpleDeckLoader = @ptrCast(@alignCast(ptr));
          // Implementation with configurable delimiter
      }
  };

  2.2 Make matching configurable:

  // src/core/matcher.zig
  pub const MatcherConfig = struct {
      case_sensitive: bool = false,
      threshold_short: usize = 1,
      threshold_long: usize = 2,
      threshold_boundary: usize = 3,
  };

  pub const Matcher = struct {
      config: MatcherConfig,

      pub fn checkAnswer(
          self: *const Matcher,
          allocator: std.mem.Allocator,
          expected: []const u8,
          actual: []const u8
      ) !MatchType {
          // Use self.config instead of hardcoded values
      }
  };

  ---
  3. Extensibility

  Major Issues:

  TrainingSession is inflexible (root.zig:83-237):
  - Hardcoded requirement: 2 correct answers (line 137)
  - Single algorithm implementation
  - Can't plug in different SRS algorithms (SM-2, FSRS, etc.)

  No configuration system:
  - All behavior is hardcoded
  - Can't customize without recompiling

  TUI is the only interface:
  - Can't easily add web UI, API, or CLI mode

  Recommendations:

  3.1 Abstract the SRS algorithm:

  // src/core/algorithm.zig
  pub const SRSAlgorithm = struct {
      ptr: *anyopaque,
      vtable: *const VTable,

      pub const VTable = struct {
          processAnswer: *const fn (
              ptr: *anyopaque,
              item: *TrackedItem,
              match_type: MatchType
          ) void,
          shouldRepeat: *const fn (ptr: *anyopaque, item: TrackedItem) bool,
          getNextInterval: *const fn (ptr: *anyopaque, item: TrackedItem) u32,
      };
  };

  // Simple algorithm (current behavior)
  pub const SimpleAlgorithm = struct {
      required_correct: u8 = 2,

      fn processAnswer(ptr: *anyopaque, item: *TrackedItem, match_type: MatchType) void {
          const self: *SimpleAlgorithm = @ptrCast(@alignCast(ptr));
          switch (match_type) {
              .exact, .fuzzy => {
                  item.correct_count += 1;
                  if (item.correct_count >= self.required_correct) {
                      item.completed = true;
                  }
              },
              .incorrect => {},
          }
      }
  };

  // Future: SM-2 algorithm
  pub const SM2Algorithm = struct {
      // ... SM-2 specific fields
  };

  3.2 Add configuration:

  // src/config.zig
  pub const Config = struct {
      // Session settings
      required_correct_answers: u8 = 2,
      shuffle_on_start: bool = true,
      reshuffle_incomplete: bool = true,

      // Matching settings
      matcher: MatcherConfig = .{},

      // Algorithm settings
      algorithm_type: AlgorithmType = .simple,

      pub fn loadFromFile(alloc: std.mem.Allocator, path: []const u8) !Config {
          // Load from TOML/JSON/etc
      }
  };

  3.3 Refactor TrainingSession:

  // src/core/session.zig
  pub const TrainingSession = struct {
      allocator: std.mem.Allocator,
      tracked_items: []TrackedItem,
      current_index: usize,
      completed_count: usize,
      algorithm: SRSAlgorithm,  // Pluggable!
      config: Config,

      pub fn init(
          allocator: std.mem.Allocator,
          items: []const Item,
          algorithm: SRSAlgorithm,
          config: Config,
      ) !TrainingSession {
          // ...
      }
  };

  ---
  4. Code Quality Issues

  Specific Problems:

  tui.zig:21-23, 89-93: Commented-out code
  // const ptr = maybe_ptr orelse return;
  // const self: *Model = @ptrCast(@alignCast(ptr));
  // self.count +|= 1;
  Action: Remove dead code

  root.zig:189: Silent error handling
  var incomplete_items = self.allocator.alloc(TrackedItem, incomplete_count) catch return;
  Action: Propagate errors or log them properly

  root.zig:70-72: Unused parameter
  pub fn calculateNextInterval(correct_count: u8) u32 {
      _ = correct_count;
      return 1;
  }
  Action: Either implement or remove if not needed

  Magic numbers throughout:
  - Line 137: if (tracked_item.correct_count >= 2)
  - Line 61: const threshold = if (normalized_expected.len > 3) @as(usize, 2)
  - Line 174: ctx.withConstraints(ctx.min, .{ .width = 62, .height = 3 })

  Action: Extract to named constants

  ---
  5. Error Handling

  Issues:

  - reshuffleIncomplete silently fails (root.zig:189)
  - Limited error context throughout
  - No custom error types for domain-specific errors

  Recommendations:

  // src/errors.zig
  pub const Error = error{
      SessionCompleted,
      InvalidDeckFormat,
      ItemNotFound,
      ConfigurationError,
      AllocatorFailure,
  };

  pub const ErrorContext = struct {
      location: []const u8,
      details: []const u8,
  };

  ---
  6. Testing

  Current State:

  - Only one basic test (root.zig:240-252)
  - No TrainingSession tests
  - No edge case testing
  - No TUI tests

  Recommendations:

  // src/core/session_test.zig
  test "TrainingSession completes after required correct answers" {
      // ...
  }

  test "TrainingSession reshuffles incomplete items" {
      // ...
  }

  test "TrainingSession handles empty item list" {
      // ...
  }

  // src/core/matcher_test.zig
  test "Matcher handles various fuzzy match scenarios" {
      // ...
  }

  test "Matcher respects configuration" {
      // ...
  }

  ---
  7. Priority Recommendations

  High Priority (Immediate Impact):

  1. Extract file loading into src/io/loader.zig with interface pattern
  2. Create config system in src/config.zig
  3. Move constants to configuration (remove magic numbers)
  4. Clean up dead code (remove commented sections)
  5. Fix error handling in reshuffleIncomplete

  Medium Priority (Next Sprint):

  6. Split root.zig into separate modules (session, matcher, algorithm)
  7. Abstract SRS algorithm to support future implementations
  8. Add comprehensive tests for TrainingSession
  9. Implement proper logging instead of silent failures
  10. Make Matcher configurable with thresholds

  Low Priority (Future):

  11. Add CLI mode implementation
  12. Support multiple deck formats (Anki, CSV, JSON)
  13. Implement SM-2 algorithm as alternative
  14. Add statistics tracking
  15. Create plugin system for custom algorithms

  ---
  8. Example Refactoring

  Here's how the new architecture would look in main.zig:

  pub fn main() !void {
      var gpa = std.heap.GeneralPurposeAllocator(.{}){};
      defer _ = gpa.deinit();
      const alloc = gpa.allocator();

      // Load configuration
      const config = Config.loadFromFileOrDefault(alloc, ".turbo-m.toml") catch Config{};

      // Parse arguments
      const parsed_args = try cli.parseArgs(alloc);

      // Create deck loader based on file extension
      var loader = try io.createLoader(alloc, parsed_args.file_path);
      defer loader.deinit();

      // Load items
      const items = try loader.load(alloc, parsed_args.file_path);
      defer {
          for (items) |item| {
              alloc.free(item.front);
              alloc.free(item.back);
          }
          alloc.free(items);
      }

      // Create algorithm
      const algorithm = createAlgorithm(config.algorithm_type, config);

      // Run in selected mode
      switch (parsed_args.mode) {
          .tui => try commands.runTui(alloc, items, algorithm, config),
          .cli => try commands.runCli(alloc, items, algorithm, config),
          .help => commands.printUsage(),
      }
  }

  ---
  Summary

  Your code is well-structured for a first iteration, but needs architectural improvements for long-term maintainability. Focus on:

  1. Separation - Split concerns into focused modules
  2. Abstraction - Use interfaces for pluggable behavior
  3. Configuration - Move hardcoded values to config
  4. Testing - Add comprehensive test coverage
  5. Error Handling - Don't silently swallow errors

  These changes will make it much easier to add features like:
  - Different SRS algorithms (SM-2, FSRS)
  - Multiple UI modes (CLI, web, API)
  - Various deck formats (Anki, CSV, JSON)
  - Advanced features (statistics, scheduling, synchronization)