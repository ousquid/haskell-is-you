FROM haskell:8.8

ARG USERNAME=haskell
ARG USER_UID=1000
ARG USER_GID=$USER_UID

RUN groupadd --gid $USER_GID $USERNAME \
    && useradd --uid $USER_UID --gid $USER_GID --shell /bin/bash -m $USERNAME

RUN apt update && apt install -y \
    libghc-opengl-dev \
    freeglut3-dev \
    libssl1.1 \
    libkrb5-3 \
    zlib1g \
    libicu[0-9][0-9] \
    gnome-keyring \
    libsecret-1-0 \
    desktop-file-utils \
    x11-utils

ENV DISPLAY=host.docker.internal:0.0

USER $USERNAME
