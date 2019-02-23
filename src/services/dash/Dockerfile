FROM node:10 as build

WORKDIR /app

COPY package.json yarn.lock ./
RUN yarn install
COPY . .
RUN yarn build

FROM mhart/alpine-node:base-10
ENV NODE_ENV production
WORKDIR /app

COPY --from=build /app/server.js /app/
COPY --from=build /app/build /app/build/
COPY --from=build /app/node_modules /app/node_modules/

EXPOSE 8080
ENTRYPOINT ["node", "server.js"]
