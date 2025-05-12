import yfinance as yf

def main():
    # List of Dow Jones 30 tickers on Yahoo Finance
    tickers = [
        "AAPL", "AMGN", "AMZN", "AXP", "BA", "CAT", "CRM", "CSCO", "CVX", "DIS",
        "GS", "HD", "HON", "IBM", "JNJ", "JPM", "KO", "MCD", "MMM", "MRK",
        "MSFT", "NKE", "NVDA", "PG", "SHW", "TRV", "UNH", "V", "VZ", "WMT"
    ]

    # Define date range
    start_date = "2024-08-01"
    end_date = "2024-12-31"

    # Download historical data for all tickers
    data = yf.download(tickers, start=start_date, end=end_date)

    if data is None:
        print("Download Failed")
        return

    # Extract Close prices
    close = data['Close']

    # Output CSV filename
    output_filename = f"dowjones30_{start_date}_to_{end_date}.csv"

    # Save to CSV
    close.to_csv(output_filename)
    print(f"Saved close prices to {output_filename}")

if __name__ == "__main__":
    main()
