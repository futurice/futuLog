import React from "react";
import { styled, Box } from "@material-ui/core";
import { Theme } from "app/ui/ux/theme";

const DEFAULT_AVATAR_SIZE = "2.5rem";

type ImageProps = React.ImgHTMLAttributes<HTMLImageElement>;

const Container = styled(Box)<Theme, Pick<ImageProps, "width">>({
  position: "relative",
  width: ({ width = DEFAULT_AVATAR_SIZE }) => width,
  height: ({ width = DEFAULT_AVATAR_SIZE }) => width,
  borderRadius: "50%",
  background: "linear-gradient(180deg, #FBE6E9 0%, #3812CE 100%)",
});

const Image = styled("img")({
  position: "absolute",
  top: 0,
  left: 0,
  bottom: 0,
  right: 0,
});

export const AvatarIcon: React.FC<ImageProps> = (props) => (
  <Container className="AvatarIcon" width={props.width} aria-hidden={true}>
    {props.src ? <Image {...props} /> : null}
  </Container>
);
