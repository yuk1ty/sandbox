const express = require("express");
const app = express();
const port = 3000;

app.get("/", (req, res, next) => {
    res.end("Top Page");
});

app.use(express.static("public"));

app.listen(port, () => {
    console.log(`Server is running on http://localhost:${port}`);
});
