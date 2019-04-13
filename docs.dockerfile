FROM node:11-alpine

WORKDIR /workspace

COPY docs/.npmrc docs/package.json docs/yarn.lock /workspace/docs/

RUN cd docs && yarn install

COPY examples examples/
COPY docs docs/

WORKDIR /workspace/docs/
RUN yarn build

ENV PORT=80
CMD ["yarn", "start"]
