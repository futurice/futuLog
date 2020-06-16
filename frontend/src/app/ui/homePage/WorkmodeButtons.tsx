import React from "react";
import { styled, Theme } from "@material-ui/core/styles";
import { Workmode, IWorkmodeDto } from "app/services/apiClientService";
import { colors } from "app/ui/ux/colors";
import { IconHome, IconOffice, IconClient, IconLeave } from "app/ui/ux/icons";
import { ButtonProps, IconButton } from "app/ui/ux/Button";

interface IWorkmodeButton extends ButtonProps {
  hoverColor: keyof typeof colors;
  active?: boolean;
}

const WorkmodeButton = styled(({ hoverColor, active, ...props }) => <IconButton {...props} />)<
  Theme,
  IWorkmodeButton & ButtonProps
>({
  width: "100%",
  padding: "0.75rem",
  border: `1px solid ${colors["deep-blue-medium"]}`,
  background: ({ hoverColor, active }) => (active ? colors[hoverColor] : colors.white),
  fontWeight: ({ active }: IWorkmodeButton) => (active ? "bold" : "normal"),

  "&:focus": {
    borderWidth: `2px`,
  },

  "&:hover": {
    backgroundColor: (props: IWorkmodeButton) => colors[props.hoverColor],
  },
});

interface IWorkmodeButtons {
  workmode: IWorkmodeDto;
  officeCapacity: number;
  disabled?: boolean;
  onSelectWorkmode: (workmode: IWorkmodeDto) => any;
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
  width: "12rem",
});

export const WorkmodeButtons: React.FC<IWorkmodeButtons> = ({
  workmode,
  officeCapacity,
  disabled,
  onSelectWorkmode,
}) => {
  const isOfficeConfirmed = workmode.type === Workmode.Office && workmode.confirmed;

  return (
    <List>
      {!isOfficeConfirmed && (
        <Item>
          <WorkmodeButton
            disabled={disabled}
            active={workmode.type === Workmode.Home}
            startIcon={<IconHome />}
            hoverColor="radical-red-light"
            onClick={() => onSelectWorkmode({ type: Workmode.Home })}
          >
            Home
          </WorkmodeButton>
        </Item>
      )}
      <Item>
        <WorkmodeButton
          disabled={disabled || isOfficeConfirmed || officeCapacity <= 0}
          active={workmode.type === Workmode.Office}
          startIcon={<IconOffice />}
          hoverColor="jade-green-light"
          onClick={() => onSelectWorkmode({ type: Workmode.Office, confirmed: false })}
        >
          Office{officeCapacity <= 0 ? ` - full!` : ""}
        </WorkmodeButton>
      </Item>
      {!isOfficeConfirmed && (
        <>
          <Item>
            <WorkmodeButton
              disabled={disabled}
              active={workmode.type === Workmode.Client}
              startIcon={<IconClient />}
              hoverColor="golden-rod-light"
              onClick={() => onSelectWorkmode({ type: Workmode.Client, name: "<Unknown>" })}
            >
              Client
            </WorkmodeButton>
          </Item>
          <Item>
            <WorkmodeButton
              disabled={disabled}
              active={workmode.type === Workmode.Leave}
              startIcon={<IconLeave />}
              hoverColor="viking-light"
              onClick={() => onSelectWorkmode({ type: Workmode.Leave })}
            >
              Leave
            </WorkmodeButton>
          </Item>
        </>
      )}
    </List>
  );
};
