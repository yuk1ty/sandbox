// prisma/seed.ts

import { PrismaClient } from "@prisma/client";
const prisma = new PrismaClient();

async function main() {
  // Delete all `User` and `Message` records
  await prisma.message.deleteMany({});
  await prisma.user.deleteMany({});
  // (Re-)Create dummy `User` and `Message` records
  await prisma.user.create({
    data: {
      name: "Jack",
      messages: {
        create: [
          {
            body: "A Note for Jack",
          },
          {
            body: "Another note for Jack",
          },
        ],
      },
    },
  });
  await prisma.user.create({
    data: {
      name: "Ryan",
      messages: {
        create: [
          {
            body: "A Note for Ryan",
          },
          {
            body: "Another note for Ryan",
          },
        ],
      },
    },
  });
  await prisma.user.create({
    data: {
      name: "Adam",
      messages: {
        create: [
          {
            body: "A Note for Adam",
          },
          {
            body: "Another note for Adam",
          },
        ],
      },
    },
  });
}

main().then(() => {
  console.log("Data seeded...");
});
