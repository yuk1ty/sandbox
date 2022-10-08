import type { GetUsersQuery } from "./graphql/generated";

export type Message = GetUsersQuery["users"][0]["messages"][0];

export type User = GetUsersQuery["users"][0];
