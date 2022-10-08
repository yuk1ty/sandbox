import { createServer } from "@graphql-yoga/node";
import { schema } from "./schema";

const port = Number(process.env.API_PORT) || 4000;
const server = createServer({ port, schema });
server.start().then(() => {
  console.log(`🚀 GraphSQL Server ready at http://localhost:${port}/graphql`);
});
