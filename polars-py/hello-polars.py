import operator
import datetime as dt
import numpy as np
import polars as pl
import polars.selectors as cs

# %%
df = pl.DataFrame({
    'name': ['Alice Archer', 'Ben Brown', 'Chloe Cooper', 'Daniel Donovan'],
    'birthdate': [
        dt.date(1997, 1, 10),
        dt.date(1985, 2, 15),
        dt.date(1983, 3, 22),
        dt.date(1981, 4, 30),
    ],
    'weight': [57.9, 72.5, 53.6, 83.1],
    'height': [1.56, 1.77, 1.65, 1.75],
})

print(df)

# %%
df.write_csv('output.csv')
df_csv = pl.read_csv('output.csv', try_parse_dates=True)
print(df_csv)

# %%
df.select(
    pl.col('name'),
    pl.col('birthdate').dt.year().alias('birth_year'),
    (pl.col('weight') / pl.col('height') ** 2).alias('bmi'),
)

# %%
df.select(
    pl.col('name'),
    (pl.col('weight', 'height') * 0.95).round(2).name.suffix('-5%'),
)

# %%
df.with_columns(
    birth_year=pl.col('birthdate').dt.year(),
    bmi=pl.col('weight') / pl.col('height') ** 2,
)

# %%
df.filter(pl.col('birthdate').dt.year() < 1990)

# %%
df.filter(
    pl.col('birthdate').is_between(dt.date(1982, 12, 31), dt.date(1996, 1, 1)),
    pl.col('height') > 1.7,    
)

# %%
df.group_by(
    (pl.col('birthdate').dt.year() // 10 * 10).alias('decade'),
    maintain_order=True,
).len()

# %%
df.group_by(
    (pl.col('birthdate').dt.year() // 10 * 10).alias('decade'),
    maintain_order=True,
).agg(
    pl.len().alias('sample_size'),
    pl.col('weight').mean().round(2).alias('avg_weight'),
    pl.col('height').max().alias('tallest'),
)

# %%
(
    df.with_columns(
        (pl.col('birthdate').dt.year() // 10 * 10).alias('decade'),
        pl.col('name').str.split(by=' ').list.first(),
    )
    .select(
        pl.all().exclude('birthdate'),
    )
    .group_by(
        pl.col('decade'),
        maintain_order=True,
    )
    .agg(
        pl.col('name'),
        pl.col('weight', 'height').mean().round(2).name.prefix('avg_'),
    )
)

# %%
df2 = pl.DataFrame({
    'name': ['Ben Brown', 'Daniel Donovan', 'Alice Archer', 'Chloe Cooper'],
    'parent': [True, False, False, False],
    'siblings': [1, 2, 3, 4],
})

df.join(df2, on='name', how='left')

# %%
df3 = pl.DataFrame({
    'name': ['Ethan Edwards', 'Fiona Foster', 'Grace Gibson', 'Henry Harris'],
    'birthdate': [
        dt.date(1977, 5, 10),
        dt.date(1975, 6, 23),
        dt.date(1973, 7, 22),
        dt.date(1971, 8, 3),
    ],
    'weight': [67.9, 72.5, 57.6, 93.1],
    'height': [1.76, 1.6, 1.66, 1.8],
})

pl.concat([df, df3], how='vertical')

# %%
q = (
     pl.scan_csv('data/iris.csv')
     .filter(pl.col('sepal_length') > 5)
     .group_by('species')
     .agg(pl.col('sepal_width').mean())
)
print(q.explain())

# %%
np.random.seed(42)

df = pl.DataFrame({
    'nrs': [1, 2, 3, None, 5],
    'names': ['foo', 'ham', 'spam', 'egg', 'spam'],
    'random': np.random.rand(5),
    'groups': ['A', 'A', 'B', 'A', 'B'],
})

df.select(
    pl.col('names').unique(maintain_order=True).alias('name'),
    pl.col('names').unique_counts().alias('count'),
)

# %%
long_df = pl.DataFrame({'numbers': np.random.randint(0, 100_000, 100_000)})
long_df.select(
    pl.col('numbers').n_unique().alias('unique'), 
    pl.col('numbers').approx_n_unique().alias('approx'),
)

# %%
df = pl.DataFrame(
    {  # As of 14th October 2024, ~3pm UTC
        "ticker": ["AAPL", "NVDA", "MSFT", "GOOG", "AMZN"],
        "company_name": ["Apple", "NVIDIA", "Microsoft", "Alphabet (Google)", "Amazon"],
        "price": [229.9, 138.93, 420.56, 166.41, 188.4],
        "day_high": [231.31, 139.6, 424.04, 167.62, 189.83],
        "day_low": [228.6, 136.3, 417.52, 164.78, 188.44],
        "year_high": [237.23, 140.76, 468.35, 193.31, 201.2],
        "year_low": [164.08, 39.23, 324.39, 121.46, 118.35],
    }
)

eur_usd_rate = 1.09  # As of 14th October 2024
gbp_usd_rate = 1.31  # As of 14th October 2024

df.with_columns((pl.col('price', '^.+_(low|high)$') / eur_usd_rate).round(2))

# %%
df.select(
    (pl.col('^year_.+$') / eur_usd_rate).name.prefix('in_eur_'),
    (pl.col('day_high', 'day_low') / gbp_usd_rate).name.suffix('_gbp'),
)

# %%
def amplitude_expressions(time_periods):
    for tp in time_periods:
        yield (pl.col(f'{tp}_high') - pl.col(f'{tp}_low')).alias(f'{tp}_amplitude')


df.with_columns(amplitude_expressions(['day', 'year']))

# %%
print(cs.expand_selector(df, cs.ends_with('_high') | cs.ends_with('_low')))
print(cs.expand_selector(df, cs.contains('_') - cs.string()))
print(cs.is_selector(cs.contains('_')))
print(cs.contains('_').as_expr())

# %%
weather = pl.DataFrame(
    {
        "station": [f"Station {idx}" for idx in range(1, 6)],
        "temperatures": [
            "20 5 5 E1 7 13 19 9 6 20",
            "18 8 16 11 23 E2 8 E2 E2 E2 90 70 40",
            "19 24 E9 16 6 12 10 22",
            "E2 E0 15 7 8 10 E1 24 17 13 6",
            "14 8 E0 16 22 24 E1",
        ],
    }
)

weather = weather.with_columns(pl.col('temperatures').str.split(' '))
weather.with_columns(
    pl.col('temperatures')
    .list.eval(pl.element().cast(pl.Int32, strict=False).is_null())
    .list.sum()
    .alias('errors'),
    pl.col('temperatures')
    .list.eval(pl.element().str.contains('(?i)[a-z]'))
    .list.sum()
    .alias('errors_2'),
)

# %%
weather_by_day = pl.DataFrame(
    {
        "station": [f"Station {idx}" for idx in range(1, 11)],
        "day_1": [17, 11, 8, 22, 9, 21, 20, 8, 8, 17],
        "day_2": [15, 11, 10, 8, 7, 14, 18, 21, 15, 13],
        "day_3": [16, 15, 24, 24, 8, 23, 19, 23, 16, 10],
    }
)

rank_pct = (pl.element().rank(descending=True) / pl.all().count()).round(2)
weather_by_day.with_columns(
    pl.concat_list(pl.all().exclude('station'))
    .list.eval(rank_pct, parallel=True)
    .alias('temps_rank'),
)

# %%
ratings = pl.DataFrame(
    {
        "Movie": ["Cars", "IT", "ET", "Cars", "Up", "IT", "Cars", "ET", "Up", "Cars"],
        "Theatre": ["NE", "ME", "IL", "ND", "NE", "SD", "NE", "IL", "IL", "NE"],
        "Avg_Rating": [4.5, 4.4, 4.6, 4.3, 4.8, 4.7, 4.5, 4.9, 4.7, 4.6],
        "Count": [30, 27, 26, 29, 31, 28, 28, 26, 33, 28],
    }
)

ratings.select(pl.col('Theatre').value_counts(sort=True)).unnest('Theatre')

# %%
rating_series = pl.Series(
    "ratings",
    [
        {"Movie": "Cars", "Theatre": "NE", "Avg_Rating": 4.5},
        {"Movie": "Toy Story", "Theatre": "ME", "Avg_Rating": 4.9},
    ],
)

rating_series.struct.rename_fields(['Film', 'State', 'Value']) \
    .to_frame().unnest('ratings')
    
# %%
(
    ratings.filter(pl.struct('Movie', 'Theatre').is_duplicated())
    .with_columns(
        pl.struct('Count', 'Avg_Rating')
        .rank('dense', descending=True)
        .over('Movie', 'Theatre')
        .alias('Rank')
    )
)

# %%
df = pl.DataFrame({'value': [1.0, 2, float('nan')]})
df.select(pl.col('value').fill_nan(None).mean())

# %%
df = pl.read_csv('data/legislators-historical.csv', schema_overrides={
    "first_name": pl.Categorical,
    "gender": pl.Categorical,
    "type": pl.Categorical,
    "state": pl.Categorical,
    "party": pl.Categorical,
})
df = df.with_columns(pl.col('birthday').str.to_date(strict=False))

# %%
(
    df.lazy()
    .group_by('first_name')
    .agg(
        pl.len(),
        pl.col('gender'),
        pl.first('last_name'),
    )
    .sort('len', descending=True)
    .limit(5)
    .collect()
)

# %%
(
    df.lazy()
    .group_by(pl.col('state'))
    .agg(
        (pl.col('party') == 'Pro-Administration').sum().alias('pro'),
        (pl.col('party') == 'Anti-Administration').sum().alias('anti'),
    )
    .sort('pro', descending=True)
    .limit(5)
    .collect()
)

# %%
def compute_age():
    return dt.date.today().year - pl.col('birthday').dt.year()


def avg_birthday(gender):
    return (
        compute_age()
        .filter(pl.col('gender') == gender)
        .mean()
        .alias(f'avg {gender} birthday')
    )


(
    df.lazy()
    .group_by('state')
    .agg(
        avg_birthday('M'),
        avg_birthday('F'),
        (pl.col('gender') == 'M').sum().alias('# male'),
        (pl.col('gender') == 'F').sum().alias('# female'),
    )
    .filter(pl.col('state').is_in(['IL', 'MA', 'NH', 'SD', 'SC']))
    .collect()
)

# %%
(
    df.lazy()
    .filter((pl.col('party') == 'Pro-Administration') | (pl.col('party') == 'Anti-Administration'))
    .group_by('state', 'party')
    .agg(pl.len().alias('count'))
    .sort('count', descending=True)
    .limit(5)
    .collect()
)

# %%
(
    df.lazy()
    .group_by('state', 'gender')
    .agg(
        compute_age().mean().alias('avg birthday'),
        pl.len().alias('#'),
    )
    .sort('#', descending=True)
    .limit(5)
    .collect()
)

# %%
def get_name():
    return pl.col('first_name') + pl.lit(' ') + pl.col('last_name')


(
    df.lazy()
    .sort('birthday', descending=True)
    .group_by('state')
    .agg(
        get_name().first().alias('youngest'),
        get_name().last().alias('oldest'),
        get_name().sort().first().alias('alphabetical_first'),
        pl.col('gender').sort_by(get_name()).first(),
    )
    .limit(5)
    .collect()
)

# %%
types = (
    "Grass Water Fire Normal Ground Electric Psychic Fighting Bug Steel "
    "Flying Dragon Dark Ghost Poison Rock Ice Fairy".split()
)
type_enum = pl.Enum(types)
df = pl.read_csv("data/pokemon.csv").cast({"Type 1": type_enum, "Type 2": type_enum})
df

# %%
df.select(
    pl.col('Name', 'Type 1'),
    pl.col('Speed').rank('dense', descending=True).over('Type 1').alias('Speed rank'),
)

# %%
(
    df.lazy()
    .group_by('Type 1')
    .agg(
        pl.col('Name'),
        pl.col('Speed').rank('dense', descending=True).alias('Speed rank'),
    )
    .explode('Name', 'Speed rank')
    .collect()
)

# %%
df.select(
    pl.col('Name', 'Type 1', 'Speed'),
    pl.col('Speed').mean().over('Type 1').alias('Mean speed in group'),
)

# %%
(
   df.lazy()
   .sort('Type 1')
   .select(
       pl.col('Type 1').head(3).over('Type 1', mapping_strategy='explode'),
       
       pl.col('Name')
       .sort_by('Speed', descending=True)
       .head(3)
       .over('Type 1', mapping_strategy='explode')
       .alias('fastest/group'),
       
       pl.col('Name')
       .sort_by('Attack', descending=True)
       .head(3)
       .over('Type 1', mapping_strategy='explode')
       .alias('strongest/group'),
       
       pl.col('Name')
       .sort()
       .head(3)
       .over('Type 1', mapping_strategy='explode')
       .alias('sorted_by_alphabet'),
    )
   .collect()
)

# %%
df = pl.DataFrame({
    'athlete': ['A', 'B', 'C', 'D', 'E', 'F'],
    'country': ['PT', 'NL', 'NL', 'PT', 'PT', 'NL'],
    'rank': [6, 1, 5, 4, 2, 3],
})

df.select(
    pl.all()
    .sort_by('rank')
    .over('country', mapping_strategy='explode')
)

# %%
df = pl.DataFrame(
    {
        "label": ["foo", "bar", "spam"],
        "a": [1, 2, 3],
        "b": [10, 20, 30],
    }
)

df.select(
    pl.fold(
        acc=pl.lit(0),
        function=operator.add,
        exprs=pl.col('a', 'b'),
    ).alias('sum_fold'),
    pl.sum_horizontal('a', 'b').alias('sum_horz'),
    (pl.col('a') + pl.col('b')).alias('sum_expr'),
)

# %%
df = pl.DataFrame(
    {
        "a": [1, 2, 3],
        "b": [0, 1, 2],
    }
)

df.filter(pl.fold(acc=pl.lit(True), function=lambda acc, x: acc & x, exprs=pl.all() > 1))

# %%
df = pl.DataFrame(
    {
        "a": ["a", "b", "c"],
        "b": [1, 2, 3],
    }
)

df.select(pl.concat_str('a', 'b'))

# %%
df = pl.DataFrame(
    {
        "keys": ["a", "a", "b", "b"],
        "values": [10, 7, 1, 23],
    }
)

# %%
def diff_from_mean(series):
    mean = sum(series) / len(series)
    return pl.Series([value - mean for value in series])


df.select(pl.col('values').map_batches(diff_from_mean))

# %%
df.group_by('keys').agg(pl.col('values').map_batches(diff_from_mean))

# %%
df_props = pl.DataFrame({
    'property_name': ['Old Ken Road', 'Whitechapel Road', 'The Shire', 'Kings Cross Station', 'The Angel, Islington'],
    'group': ['brown', 'brown', 'fantasy', 'stations', 'light_blue'],
})

df_prices = pl.DataFrame({
    'property_name': ['Old Ken Road', 'Whitechapel Road', 'Sesame Street', 'Kings Cross Station', 'The Angel, Islington'],
    'cost': [60, 60, 100, 200, 100],
})

df_props.join(df_prices, on='property_name', how='full', coalesce=True)

# %%
(
    df_props
    .with_columns(pl.col('property_name').str.to_lowercase())
    .join(
        df_prices.select(
            pl.col('property_name').alias('name'), 
            pl.col('cost'),
        ),
        left_on='property_name',
        right_on=pl.col('name').str.to_lowercase(),
    )
)

# %%
df_players = pl.DataFrame({
    'name': ['Alice', 'Bob'],
    'cash': [78, 135],
})

df_players.join_where(df_prices, pl.col('cash') > pl.col('cost'))

# %%
df_trades = pl.DataFrame(
    {
        "time": [
            dt.datetime(2020, 1, 1, 9, 1, 0),
            dt.datetime(2020, 1, 1, 9, 1, 0),
            dt.datetime(2020, 1, 1, 9, 3, 0),
            dt.datetime(2020, 1, 1, 9, 6, 0),
        ],
        "stock": ["A", "B", "B", "C"],
        "trade": [101, 299, 301, 500],
    }
)

df_quotes = pl.DataFrame(
    {
        "time": [
            dt.datetime(2020, 1, 1, 9, 0, 0),
            dt.datetime(2020, 1, 1, 9, 2, 0),
            dt.datetime(2020, 1, 1, 9, 4, 0),
            dt.datetime(2020, 1, 1, 9, 6, 0),
        ],
        "stock": ["A", "B", "C", "A"],
        "quote": [100, 300, 501, 102],
    }
)

df_trades.join_asof(df_quotes, on='time', by='stock', strategy='backward', tolerance='1m')

# %%
tokens = pl.DataFrame({"monopoly_token": ["hat", "shoe", "boat"]})
df_players.select(pl.col('name')).join(tokens, how='cross')
