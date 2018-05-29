var conn = new Mongo();
db = conn.getDB("test");
db.testCollection.insert([
    {
	user: "Alice",
	gender: "f"
    },
    {
	user: "Bob",
	gender: "m"
    }
]);
