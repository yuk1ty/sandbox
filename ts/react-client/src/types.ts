export type Message = {
  body: string;
};

export type User = {
  name: string;
  messages: Message[];
};
