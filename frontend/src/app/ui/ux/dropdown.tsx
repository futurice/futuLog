import React, { useState } from 'react';
import { styled } from '@material-ui/core/styles';
import { Select as MuiSelect } from '@material-ui/core';
import { SelectProps as MuiSelectProps } from '@material-ui/core/Select';
import MenuItem from '@material-ui/core/MenuItem';
import InputLabel from '@material-ui/core/InputLabel';
import FormControl from '@material-ui/core/FormControl';
import { makeStyles } from '@material-ui/core/styles';

import { colors, theme } from './theme';
import { IconArrowUp } from './icons';


export const DropDown = styled((props: MuiSelectProps) => <MuiSelect {...props} />)({
  color: `${colors['deep-blue-80']}`,
  border: `1px solid ${colors['deep-blue-80']}`,
  boxSizing: 'border-box',
  borderRadius: '4px',
  backgroundColor: `${colors['deep-blue-80']}`,
  '&:hover': {
    border: '1px solid #6643EF',
    backgroundColor: `${colors['deep-blue-50']}`,
  },
  '&:active': {
    backgroundColor: `${colors['deep-blue-50']}`,
  },
  '&:disabled': {
    background: 'rgba(255, 255, 255, 0.5)',
    border: '1px solid rgba(166, 157, 199, 0.5)',
    color: 'rgba(255, 255, 255, 0.5)',
    backgroundColor: 'rgba(32, 10, 116, 0.5)',
  },
  '&.MuiSelect-icon': {
    background: `${colors['deep-blue-80']}`,
    color: `${colors['white']}`,
  },
  list: {
    paddingTop: 0,
    paddingBottom: 0,
    background: 'white',
    '& li': {
      fontWeight: 200,
      paddingTop: 12,
      paddingBottom: 12,
    },
    '& li:hover': {
      background: `${colors['deep-blue-80']}`
    },
    '& li.Mui-selected': {
      color: 'white',
      background: `${colors['deep-blue-50']}`
    },
    '& li.Mui-selected:hover': {
      background: `${colors['deep-blue-80']}`
    }
  },
  icon: {
    background: `${colors['deep-blue-80']}`,
  },
  '& .MuiSelect-selectMenu':{
    backgroundColor: 'white'
  }
});


const useStyles = makeStyles({
  formControl: {
    margin: theme.spacing(1),
    minWidth: 120,
    color: `${colors['deep-blue-80']}`,
    border: `1px solid ${colors['deep-blue-80']}`,
    boxSizing: 'border-box',
    borderRadius: '4px',
    '&:hover': {
      border: '1px solid #6643EF',
      backgroundColor: '#D2CEE3',
    },
    '&:active': {
      backgroundColor: '#D2CEE3',
    },
    '&:disabled': {
      background: 'rgba(255, 255, 255, 0.5)',
      border: '1px solid rgba(166, 157, 199, 0.5)',
      color: 'rgba(255, 255, 255, 0.5)'
    },
    '&.MuiSelect-icon': {
      background: `${colors['deep-blue-80']}`,
      color: `${colors['white']}`,
    },
  },
  selectEmpty: {
    marginTop: theme.spacing(2),
  },
  list: {
    paddingTop: 0,
    paddingBottom: 0,
    background: 'white',
    '& li': {
      fontWeight: 200,
      paddingTop: 12,
      paddingBottom: 12,
    },
    '& li:hover': {
      color: 'white',
      background: `${colors['deep-blue-80']}`
    },
    '& li.Mui-selected': {
      color: 'white',
      background: `${colors['deep-blue-50']}`
    },
    '& li.Mui-selected:hover': {
      color: 'white',
      background: `${colors['deep-blue-80']}`
    },
  },
  icon:{
    color: `${colors["deep-blue-50"]}`,
    right: 12,
    position: 'absolute',
    userSelect: 'none',
    pointerEvents: 'none',
    background: `${colors['deep-blue-80']}`,
  },
  button: {
    display: 'block',
    marginTop: theme.spacing(2),
  },

});

export interface entryData{
  entryValue: string;
  entryText: string;
}

function buildMenuItems(entry:entryData[]){
  return(
    entry.map((entrySet,index) => {
      return (<MenuItem value={entrySet.entryValue}>{entrySet.entryText}</MenuItem>)
    })

  )
}

export interface selectProps{
  entry: entryData[],
  label: string
}

export const Select = ({entry, label}: selectProps) => { //(entry: entryData, label: string)
  const classes = useStyles()

  const [val, setVal] = React.useState<string | number>('');
  const [open, setOpen] = React.useState(false);

  const handleChange = (event: React.ChangeEvent<{ value: unknown }>) => {
    setVal(event.target.value as number);
  };

  const handleClose = () => {
    setOpen(false);
  };

  const handleOpen = () => {
    setOpen(true);
  };

  const menuProps = {
    classes: {
      list: classes.list
    },
    anchorOrigin: {
      vertical: 1,//'bottom',
      horizontal: 1,//'left'
    },
    transformOrigin: {
      vertical: 1, //'top',
      horizontal: 1 //'left'
    },
    getContentAnchorEl: null,
  };


  console.log('open:::', open ? 'true' : 'false');

  return (
    <div>
      <FormControl className={classes.formControl}>
        <DropDown
          labelId={label}
          id={label}
          value={val}
          open={open}
          onClose={handleClose}
          onOpen={handleOpen}
          onChange={handleChange}
          IconComponent={() => open ? (<img src="/Icon-arrow-down.png" />) : (<img src="/Icon-arrow-up.png"/>)}
          MenuProps={menuProps}
        >
          {buildMenuItems(entry)}
        </DropDown>
      </FormControl>
    </div>
  );
}
