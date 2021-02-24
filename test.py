import json

with open('test.json') as f:
  data = json.load(f)

json_list = list()
for item in data:
    for situation in item["situations"]:
        situation["id"] = f"{item['id']}-{situation['runs']}"
        situation["first_base"] = item['runners']['first']
        situation["second_base"] = item['runners']['second']
        situation["third_base"] = item['runners']['third']
        situation["outs"] = int(item["outs"])
        situation["count"] = int(situation["count"])
        situation["runs"] = int(situation["runs"])
        situation["outs_category_total"] = int(item["total"])
        json_list.append(situation)

with open('test_adj.json', 'w') as json_file:
    json.dump(json_list, json_file)

