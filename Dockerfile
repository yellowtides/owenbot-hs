FROM haskell:8

WORKDIR /owen
COPY ./ .

RUN stack setup
RUN stack build

CMD [ "stack exec owenbot-exe" ]