import { builder } from "../builder";

builder.prismaObject("Message", {
  fields: (t) => ({
    id: t.exposeID("id"),
    body: t.exposeID("body"),
    createdAt: t.expose("createdAt", {
      type: "Date",
    }),
  }),
});
