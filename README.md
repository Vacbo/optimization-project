# Portfolio Optimization Project

A Haskell-based project for portfolio optimization using functional programming and parallelism.

## Project Description

This project simulates and optimizes portfolios based on stock data from the Dow Jones index. The goal is to find the optimal portfolio allocation with the highest Sharpe ratio by exploring all possible combinations of 25 stocks out of 30 Dow Jones components.

### Key Features:

- Selects 25 out of 30 stocks (~142,506 combinations)
- Simulates multiple portfolios for each combination (configurable, default 1,000)
- Calculates expected returns, risk (volatility), and Sharpe ratio
- Enforces constraints: long-only positions, maximum 20% allocation per stock
- Uses parallelism with work-stealing for efficient computation
- Implements pure functional programming concepts

## Installation

### Prerequisites

- [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) (Haskell build tool)
- GHC 9.8.4 or compatible version

### Building the Project

```bash
# Clone the repository
git clone https://github.com/yourusername/optimization-project.git
cd optimization-project

# Build the project
stack build
```

## Running the Project

### Basic Usage

```bash
stack exec optimization-project-exe -- <data-file-path> <number-of-simulations> [+RTS options]
```

### Parameters:

- `data-file-path`: Path to the CSV file containing stock data
- `number-of-simulations`: Number of simulations per stock combination (e.g., 100, 1000)

### Example:

```bash
stack exec optimization-project-exe -- data/dowjones30_2024-08-01_to_2024-12-31.csv 100 +RTS -N8 -T -s
```

### Runtime Options:

- `+RTS -N`: Use all available cores
- `+RTS -N8`: Use 8 cores (recommended for best performance on most systems)
- `+RTS -T`: Enable runtime statistics
- `+RTS -s`: Print memory usage statistics

## Project Architecture

The project is organized into several modules:

- `Main.hs`: Entry point and result display
- `Lib.hs`: Core optimization logic and coordination
- `Simulate.hs`: Portfolio simulation and metrics calculation
- `DataLoader.hs`: Loading and processing stock data
- `WorkStealing.hs`: Parallelism with work-stealing algorithm
- `MemoryEfficient.hs`: Memory-efficient operations
- `Cache.hs` and `AdvancedCache.hs`: Caching mechanisms
- `PerformanceMetrics.hs`: Performance monitoring

## Algorithm Details

1. Data is loaded from CSV files containing Dow Jones index stocks (30 companies)
2. The full covariance matrix is calculated for all 30 stocks
3. All possible combinations of 25 stocks are generated (142,506 combinations)
4. For each combination:
   - Multiple portfolio weight allocations are simulated (defaults to 1,000)
   - Each simulation generates random weights following a Dirichlet distribution
   - Constraints are enforced: no stock can exceed 20% allocation
   - Metrics are calculated: expected return, volatility, Sharpe ratio
5. The portfolios are ranked by Sharpe ratio, and the top results are displayed

## Optimizations

This project implements several optimizations for performance:

1. **Algorithmic Improvements**:
   - Optimized Dirichlet distribution for weight generation
   - Strict evaluation with bang patterns and unboxed tuples
   - Avoiding unnecessary vector operations and conversions
   - Eliminating intermediate data structures
   - INLINE pragmas for key functions

2. **Parallelism Improvements**:
   - Work stealing with larger chunk sizes for better workload distribution
   - Optimized progress tracking with reduced frequency
   - Better load balancing for optimal core utilization
   - Targeting 8 cores rather than maximum cores (due to memory bandwidth limitations)

3. **Memory Optimizations**:
   - Reduced boxed/unboxed vector conversions
   - Batch processing for better cache efficiency
   - Direct dot product implementations without temporary vectors
   - Simplified data flow to reduce allocation overhead

4. **Compiler Optimizations**:
   - -O2 optimization level
   - -funbox-strict-fields for better memory representation
   - -fexcess-precision for better numerical accuracy
   - -optc-O3 and -optc-ffast-math for better C-level optimizations

## Performance Metrics

The current optimized implementation achieves:
- Computation time: ~31 seconds (down from 80-160s in previous versions)
- Memory usage: ~201GB (down from 666GB)
- Parallelization efficiency: ~762% with 8 cores
- GC time: ~3.7 seconds (~3-4% of runtime)
- Productivity: ~97% of elapsed time

Key findings:
- 8 cores outperform 16 cores due to memory bandwidth limitations
- Memory usage is the primary constraint rather than CPU power
- Low GC overhead indicates efficient memory management

## Data Format

The program expects CSV files with daily price data for stocks. The file should include:
- A header row with stock symbols
- Daily price data for all stocks
- Dates in the first column

## License

This project is licensed under the BSD-3-Clause License - see the LICENSE file for details.
