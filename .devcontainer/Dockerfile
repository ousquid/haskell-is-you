FROM haskell:8.8

ARG USERNAME=haskell
ARG USER_UID=1000
ARG USER_GID=$USER_UID

RUN groupadd --gid $USER_GID $USERNAME \
    && useradd --uid $USER_UID --gid $USER_GID --shell /bin/bash -m $USERNAME

RUN apt update && apt install -y libghc-opengl-dev freeglut3-dev

ENV DISPLAY=host.docker.internal:0.0

USER $USERNAME
