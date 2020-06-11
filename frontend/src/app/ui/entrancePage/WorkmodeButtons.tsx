import React from "react";
import { makeStyles } from "@material-ui/core/styles";
import Button, { ButtonProps } from "@material-ui/core/Button";
import { Workmode } from "app/services/apiClientService";
import { colors } from "app/ui/ux/colors";
import { buttonStyles } from "app/ui/ux/styles";
import { IconHome, IconOffice, IconClient, IconLeave } from "app/ui/ux/icons";

interface IWorkmodeButton extends ButtonProps {
  hoverColor: keyof typeof colors;
  active?: boolean;
}

const useWorkmodeButtonStyles = makeStyles({
  root: {
    ...buttonStyles,
    background: ({ hoverColor, active }: IWorkmodeButton) =>
      active ? colors[hoverColor] : colors.white,
    border: `1px solid ${colors["deep-blue-medium"]}`,
    fontWeight: ({ active }: IWorkmodeButton) => (active ? "bold" : "normal"),

    "&:focus": {
      borderWidth: `2px`,
    },

    "&:hover": {
      backgroundColor: (props: IWorkmodeButton) => colors[props.hoverColor],
    },

    "& > .MuiButton-label": {
      justifyContent: "flex-start",
    },
    "& .MuiButton-startIcon": {
      marginLeft: 0,
    },
  },
});

const WorkmodeButton: React.FC<IWorkmodeButton> = ({ hoverColor, active, ...props }) => {
  const classes = useWorkmodeButtonStyles({ hoverColor, active });
  return <Button className={classes.root} {...props} disableRipple fullWidth />;
};

interface IWorkmodeButtons {
  workmode: Workmode;
  onSelectWorkmode: (workmode: Workmode) => any;
}

const useWorkmodeButtonsStyles = makeStyles({
  root: {
    display: "flex",
    flexWrap: "wrap",
    padding: "0",
    margin: "0.375rem -0.75rem",
    listStyle: "none",
  },
  item: {
    padding: "0.375rem 0.75rem",
    width: "50%",
  },
});

export const WorkmodeButtons: React.FC<IWorkmodeButtons> = ({ workmode, onSelectWorkmode }) => {
  const classes = useWorkmodeButtonsStyles();
  return (
    <ul className={`WorkmodeButtons ${classes.root}`}>
      <li className={classes.item}>
        <WorkmodeButton
          active={workmode === Workmode.Home}
          startIcon={<IconHome />}
          hoverColor="radical-red-light"
          onClick={() => onSelectWorkmode(Workmode.Home)}
        >
          Home office
        </WorkmodeButton>
      </li>
      <li className={classes.item}>
        <WorkmodeButton
          active={workmode === Workmode.Office}
          startIcon={<IconOffice />}
          hoverColor="jade-green-light"
          onClick={() => onSelectWorkmode(Workmode.Office)}
        >
          Office
        </WorkmodeButton>
      </li>
      <li className={classes.item}>
        <WorkmodeButton
          active={workmode === Workmode.Client}
          startIcon={<IconClient />}
          hoverColor="golden-rod-light"
          onClick={() => onSelectWorkmode(Workmode.Client)}
        >
          Client
        </WorkmodeButton>
      </li>
      <li className={classes.item}>
        <WorkmodeButton
          active={workmode === Workmode.Leave}
          startIcon={<IconLeave />}
          hoverColor="viking-light"
          onClick={() => onSelectWorkmode(Workmode.Leave)}
        >
          Leave
        </WorkmodeButton>
      </li>
    </ul>
  );
};
