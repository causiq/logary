FROM node:11-alpine

WORKDIR /workspace

COPY package.json yarn.lock /workspace/

RUN yarn install

COPY . .

RUN yarn build

ENV PORT=80
CMD ["yarn", "start"]
