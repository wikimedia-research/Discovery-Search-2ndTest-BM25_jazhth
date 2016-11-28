import feather
import tinysegmenter
import json

path = 'data/queries_ja.feather'
df = feather.read_dataframe(path)
df['query'] = df['query'].str.replace('[^\w\s]',' ')
df['query'] = df['query'].str.replace(' +',' ')
segmenter = tinysegmenter.TinySegmenter()

output = dict()
for row in df.iterrows():
  output[row[1]['page_id']] = segmenter.tokenize(row[1]['query'])


with open('data/tokens_ja.json', 'w') as fp:
    json.dump(output, fp, ensure_ascii=False)