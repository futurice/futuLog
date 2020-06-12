import React from "react";
import { styled, Theme } from "@material-ui/core/styles";
import Button, { ButtonProps } from "@material-ui/core/Button";
import { Workmode } from "app/services/apiClientService";
import { colors } from "app/ui/ux/colors";
import { buttonStyles } from "app/ui/ux/styles";
import { IconHome, IconOffice, IconClient, IconLeave } from "app/ui/ux/icons";

interface IWorkmodeButton extends ButtonProps {
  hoverColor: keyof typeof colors;
  active?: boolean;
}

const WorkmodeButton = styled((props) => <Button {...props} disableRipple fullWidth />)<
  Theme,
  IWorkmodeButton & ButtonProps
>({
  ...buttonStyles,
  background: ({ hoverColor, active }) => (active ? colors[hoverColor] : colors.white),
  border: `1px solid ${colors["deep-blue-medium"]}`,
  fontWeight: ({ active }: IWorkmodeButton) => (active ? "bold" : "normal"),

  "&:focus": {
    borderWidth: `2px`,
  },

  "&:hover": {
    backgroundColor: (props: IWorkmodeButton) => colors[props.hoverColor],
    boxShadow: "2px 2px 4px rgba(10, 3, 37, 0.2)",
  },

  "& > .MuiButton-label": {
    justifyContent: "flex-start",
  },
  "& .MuiButton-startIcon": {
    marginLeft: 0,
  },
});

interface IWorkmodeButtons {
  workmode: Workmode;
  onSelectWorkmode: (workmode: Workmode) => any;
}

const List = styled("ul")({
  display: "flex",
  padding: "0",
  margin: "-0.375rem -0.75rem",
  flexWrap: "wrap",
  justifyContent: "center",
  listStyle: "none",
});
const Item = styled("li")({
  padding: "0.375rem 0.75rem",
  width: "10rem",
});

export const WorkmodeButtons: React.FC<IWorkmodeButtons> = ({ workmode, onSelectWorkmode }) => (
  <List>
    <Item>
      <WorkmodeButton
        active={workmode === Workmode.Home}
        startIcon={<IconHome />}
        hoverColor="radical-red-light"
        onClick={() => onSelectWorkmode(Workmode.Home)}
      >
        Home
      </WorkmodeButton>
    </Item>
    <Item>
      <WorkmodeButton
        active={workmode === Workmode.Office}
        startIcon={<IconOffice />}
        hoverColor="jade-green-light"
        onClick={() => onSelectWorkmode(Workmode.Office)}
      >
        Office
      </WorkmodeButton>
    </Item>
    <Item>
      <WorkmodeButton
        active={workmode === Workmode.Client}
        startIcon={<IconClient />}
        hoverColor="golden-rod-light"
        onClick={() => onSelectWorkmode(Workmode.Client)}
      >
        Client
      </WorkmodeButton>
    </Item>
    <Item>
      <WorkmodeButton
        active={workmode === Workmode.Leave}
        startIcon={<IconLeave />}
        hoverColor="viking-light"
        onClick={() => onSelectWorkmode(Workmode.Leave)}
      >
        Leave
      </WorkmodeButton>
    </Item>
  </List>
);
