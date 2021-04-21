import React from "react";
import { IOfficeSpaceDto, ICapacityDto } from "app/services/apiClientService";
import { ButtonDiscrete, ButtonDiscreteWithEndIcon } from "app/ui/ux/buttons";
import { IconArrowDown, IconArrowUp } from "app/ui/ux/icons";
import MenuItem from "@material-ui/core/MenuItem";
import Popover from "@material-ui/core/Popover";
import Typography from "@material-ui/core/Typography";
import List from "@material-ui/core/List";
import ListItem from "@material-ui/core/ListItem";
import Box from "@material-ui/core/Box";
import ListItemText from "@material-ui/core/ListItemText";
import ListItemAvatar from "@material-ui/core/ListItemAvatar";
import { makeStyles } from "@material-ui/core/styles";
import { Select } from "app/ui/ux/select";
import { officesQueryKey } from "app/utils/reactQueryUtils";
import { useServices } from "../../services/services";
import { AvatarIcon } from "app/ui/siteLayout/AvatarIcon";

const divider = makeStyles({
  root: {
    background: "#D2CEE3",
    border: "1px solid #D2CEE3",
    boxSizing: "content-box",
    display: "inline",
    margin: "0 8px",
  },
});

const center = makeStyles({
  root: {
    boxSizing: "content-box",
    marginLeft: "auto",
    marginRight: "auto",
    flexDirection: "column",
    alignItems: "center",
    alignSelf: "center",
  },
  buttons: {
    textAlign: "center",
  },
});

const list_holder = makeStyles({
  container: {
    maxHeight: 490,
  },
  root: {
    border: "1px solid #200A74",
    boxSizing: "border-box",
    borderRadius: "4px",
  },
  listItem: {
    padding: "4px 8px",
  },
  empty: {
    padding: 10,
  },
});

const active = makeStyles({
  border: {
    border: "1px solid #200A74",
  },
});

interface OfficeControllerProps {
  userOffice: string | undefined;
  officeBookings: ICapacityDto[];
  onSelectOffice: (office: string) => void,
}

export const OfficeController = ({ userOffice, officeBookings, onSelectOffice }: OfficeControllerProps) => {
  const centerClass = center();
  const dividerClass = divider();
  const activeClass = active();

  const [officeState, setOfficeState] = React.useState(false);
  const [whoBookedState, setWhoBookedState] = React.useState(false);
  const [anchorEl, setAnchorEl] = React.useState<HTMLButtonElement | null>(null);

  const toggleOfficeStates = () => {
    setOfficeState(!officeState);
    setWhoBookedState(false);
  };

  const toggleWhoBooked = () => {
    setWhoBookedState(!whoBookedState);
    setOfficeState(false);
  };

  const handleOfficeChange = (event: React.ChangeEvent<{ name?: string; value: unknown }>) => {
    const currentOffice = event.target.value as string;
    onSelectOffice(currentOffice);
  };

  const handleClick = (event: React.MouseEvent<HTMLButtonElement>) => {
    toggleWhoBooked();
    setAnchorEl(event.currentTarget);
  };

  const handleClose = () => {
    setAnchorEl(null);
    toggleWhoBooked();
  };

  const { queryCache } = useServices();

  const offices = queryCache.getQueryData<IOfficeSpaceDto[]>(officesQueryKey());
  const open = Boolean(anchorEl);
  const id = open ? "simple-popover" : undefined;
  const officeClass = officeState ? activeClass.border : "";
  const bookedClass = whoBookedState ? activeClass.border : "";
  const bookedArrow = whoBookedState ? <IconArrowUp /> : <IconArrowDown />;
  const users = (officeBookings && officeBookings.length > 0) ? officeBookings[0].people : [];
  const list_holderClass = list_holder();
  const officesOptions = (offices || []).map(({ site }) => ({ value: site, label: site }));

  return (
    <Box>
      <Box className={centerClass.root}>
        <Box padding={"0 2rem"}>
          <ButtonDiscrete className={officeClass} onClick={toggleOfficeStates}>
            Change Office
          </ButtonDiscrete>
          <Box className={dividerClass.root} />
          <ButtonDiscreteWithEndIcon
            aria-describedby={id}
            endIcon={bookedArrow}
            className={bookedClass}
            onClick={handleClick}
          >
            who booked
          </ButtonDiscreteWithEndIcon>
        </Box>
        {whoBookedState && (
          <Popover
            id={id}
            open={whoBookedState}
            anchorEl={anchorEl}
            onClose={handleClose}
            anchorOrigin={{
              vertical: "bottom",
              horizontal: "left",
            }}
            transformOrigin={{
              vertical: "top",
              horizontal: "left",
            }}
          >
            <Box className={list_holderClass.container}>
              <List className={list_holderClass.root}>
                {users.map((user) => {
                  return (
                    <ListItem className={list_holderClass.listItem}>
                      <ListItemAvatar>
                        <AvatarIcon src={user.picture} />
                      </ListItemAvatar>
                      <ListItemText primary={user.name} />
                    </ListItem>
                  );
                })}
                {users.length === 0 && (
                  <Box className={list_holderClass.empty}>
                    <Typography variant="caption">No users booked for this day.</Typography>
                  </Box>
                )}
              </List>
            </Box>
          </Popover>
        )}
        {officeState && (
          <Box>
            <Box fontWeight="fontWeightBold">Office</Box>
            <Select
              value={userOffice}
              onChange={handleOfficeChange}
              name="range"
              inputProps={{
                id: "range-select",
              }}
            >
              {officesOptions.map(({ value, label }) => (
                <MenuItem disableRipple key={value} value={value}>
                  {label}
                </MenuItem>
              ))}
            </Select>
          </Box>
        )}
      </Box>
    </Box>
  );
};
