import polars as pl
import datetime as dt

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