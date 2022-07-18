from sqlalchemy import create_engine, Table, Column, Integer, String, MetaData

engine = create_engine("mysql://root:root@localhost/example", echo=True)

meta = MetaData()
students = Table(
    "students",
    meta,
    Column("id", Integer, primary_key=True),
    Column("name", String),
    Column("lastname", String),
)
meta.create_all(engine)

conn = engine.connect()
ins = students.insert().values(name="Ravi", lastname="Kapoor")
result = conn.execute(ins)

# get primary key
print(result.inserted_primary_key)

conn.execute(
    students.insert(),
    [
        {"name": "Rajiv", "lastname": "Khanna"},
        {"name": "Komal", "lastname": "Bhandari"},
        {"name": "Abdul", "lastname": "Sattar"},
        {"name": "Priya", "lastname": "Rajhans"},
    ],
)
