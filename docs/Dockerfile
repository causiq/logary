FROM node:11-alpine

WORKDIR /workspace

COPY package.json yarn.lock /workspace/

RUN yarn install

COPY . .

RUN yarn build

CMD ["yarn", "start"]
