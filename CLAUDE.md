# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

turbo-m is a spaced repetition learning application written in Haskell, inspired by SuperMemo. It implements the SM-2 algorithm for optimizing review intervals based on user performance.

## Build and Development Commands

- Build the project: `cabal build`
- Run the executable: `cabal run turbo-m -- <vocab_file_path>`
- Run tests: `cabal test`
- Start a REPL with project loaded: `cabal repl`
- Clean build artifacts: `cabal clean`

## Architecture

The codebase follows a modular Haskell structure:

### Core Library (`lib/TurboM.hs`)
- `Item`: Core data type representing a flashcard with spaced repetition metadata (easiness factor, repetitions, interval, due date)
- `ItemsCollection`: Container for groups of items
- `ReviewGrade`: Enum for user review feedback (Again, Hard, Good, Easy)
- `reviewItem`: SM-2 algorithm implementation for updating item state based on review performance

### Application (`app/Main.hs`)
- Interactive CLI using Haskeline for user input
- File parsing for vocabulary files with `##` separator format
- Item shuffling and training loop implementation
- String matching with normalization (case-insensitive, punctuation-stripped)

### Input Format
Vocabulary files use `##` as separator: `answer##question` (e.g., `abnehmen##to take down, to take off`)

### Key Dependencies
- `haskeline`: Interactive command-line interface
- `random-shuffle`: Item randomization
- `split`: String parsing utilities

## Language Features
Uses GHC2024 language standard with strict data and overloaded strings extensions.