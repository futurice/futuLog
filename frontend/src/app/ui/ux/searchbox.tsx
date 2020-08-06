// import React from "react";
// import { styled } from "@material-ui/core/styles";
// import { Select as MuiSelect } from "@material-ui/core";
// import { SelectProps as MuiSelectProps } from "@material-ui/core/Select";
// import { makeStyles } from "@material-ui/core/styles";

// import {InputBaseProps as MuiInputBaseProps} from "@material-ui/core/InputBase";
// import { colors } from "./theme";
// import { Flex } from "./containers";
// import { InputBase } from "./inputs"
// import { AutocompleteProps as MuiAutocompleteProps } from "@material-ui/lab/Autocomplete";
// // import  { Autocomplete as MuiAutocomplete } from "@material-ui/lab";
// import Autocomplete from "@material-ui/lab/Autocomplete";
// import { IconSearch } from "./icons";
// import { IOfficeSpaceDto, IUserDto } from "../../services/apiClientService";

// //import TextField from "@material-ui/core/TextField";
// import { TextField } from "./inputs";

// interface userSearch{
//   name:string,
//   nameLabel:number,
// }

// const top100Films = [
//   { title: 'The Shawshank Redemption', year: 1994 },
//   { title: 'The Godfather', year: 1972 },
//   { title: 'The Godfather: Part II', year: 1974 },
//   { title: 'The Dark Knight', year: 2008 },
//   { title: '12 Angry Men', year: 1957 },
//   { title: "Schindler's List", year: 1993 },
//   { title: 'Pulp Fiction', year: 1994 },
//   { title: 'The Lord of the Rings: The Return of the King', year: 2003 },
//   { title: 'The Good, the Bad and the Ugly', year: 1966 },
//   { title: 'Fight Club', year: 1999 },
//   { title: 'The Lord of the Rings: The Fellowship of the Ring', year: 2001 },
//   { title: 'Star Wars: Episode V - The Empire Strikes Back', year: 1980 },
//   { title: 'Forrest Gump', year: 1994 }, ]

// // export const Autocomplete = styled((props: MuiAutocompleteProps) => <MuiAutocomplete {...props}/>)({
// //
// //
// // })

// export function ComboBox() {
//   return (
//     <Autocomplete
//       id="combo-box-demo"
//       options={top100Films}
//       getOptionLabel={(option) => option.title}
//       style={{ width: 300 }}
//       forcePopupIcon={true}
//       popupIcon={
//           <Flex className={"MuiSelect-icon"}>
//             <IconSearch/>
//           </Flex>
//       }
//       renderInput={(params) => <TextField {...params}  />}
//     />
//   );
// }

// interface IComboBox extends Array<IUserDto>{}

import React from "react";
import { makeStyles } from "@material-ui/core/styles";
import { Autocomplete } from "@material-ui/lab";
import { AutocompleteProps as MuiAutocompleteProps } from "@material-ui/lab/Autocomplete";
import { IconArrowDown } from "./icons";
import { colors } from "./theme";

const useStyles = makeStyles(() => ({
  popupIndicator: {
    backgroundColor: `${colors["deep-blue-80"]}`,
    borderRadius: "0 3px 3px 0",
    height: "36px",
    width: "36px",
    "& .MuiSvgIcon-root": {
      fill: "#ffffff",
    },
  },
  popupIndicatorOpen: {
    "& .MuiSvgIcon-root": {
      transform: "rotate(180deg)",
    },
  },
}));

type optionList = {
  label: string;
  email?: string;
};

export const Searchbox = (props: MuiAutocompleteProps<optionList, boolean, boolean, boolean>) => {
  const classes = useStyles();
  return (
    <Autocomplete
      {...props}
      popupIcon={<IconArrowDown color={colors.white} />}
      classes={{
        popupIndicator: classes.popupIndicator,
        popupIndicatorOpen: classes.popupIndicatorOpen,
      }}
    />
  );
};
