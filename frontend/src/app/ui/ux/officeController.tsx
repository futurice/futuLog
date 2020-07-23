import React, { useState } from "react";
import {
  Workmode,
  IWorkmodeDto,
  IShiftAssignmentDto,
  IOfficeSpaceDto,
  IUserWorkmodeDto,
  IUserDto,
  IBookedDto
} from "app/services/apiClientService";
import {ButtonDiscrete, ButtonDiscreteWithEndIcon} from "app/ui/ux/buttons"
import List from '@material-ui/core/List';
import ListItem from '@material-ui/core/ListItem';
import ListItemText from '@material-ui/core/ListItemText';
import ListItemAvatar from '@material-ui/core/ListItemAvatar';
import Avatar from '@material-ui/core/Avatar';
import {makeStyles} from '@material-ui/core/styles';
import {Select, selectProps, entryData} from 'app/ui/ux/dropdown';

const divider = makeStyles({
  root:{
    background: '#D2CEE3',
    border: '1px solid #D2CEE3',
    boxSizing: 'content-box',
    display: 'inline',
  }
});

const center = makeStyles({
  root:{
    boxSizing: 'content-box',
    marginLeft: 'auto',
    marginRight: 'auto',
    textAlign: 'center',
    display: 'inline',
    flexDirection: 'column',
    alignItems: 'center',
    alignSelf: 'center',
  }
});

const list_holder = makeStyles({
  root:{
    border: '1px solid #200A74',
    boxSizing: 'border-box',
    borderRadius: '4px',
  }
})

interface OfficeControllerProps{
  userOffice: IOfficeSpaceDto,
  officeBookings:IBookedDto[]
}

function officeStateSelector(officeState:boolean){
  const office_list: string[] = ['Berlin', 'Helsinki', 'London', 'Munich', 'Oslo', 'Stockholm', 'Stuttgart', 'Tampere']
  if (officeState) {
    return (
    <List className={list_holderClass.root}>
    {officeBookings.map((users,index) => {
      return (users.users.map((user, index2) => {
        return <ListItem>
          <ListItemAvatar>
           <Avatar>
            <img src={user.portrait_badge_url}/>
           </Avatar>
          </ListItemAvatar>
          <ListItemText primary={user.first_name + user.last_name}/>
        </ListItem>

      }))
    })
  }</List> )}
  else{
    return (<Select entry={entryValue:office_list, entryText:office_list} label="Office List"/>)
  }
}
}

export const OfficeController = ({userOffice, officeBookings} :OfficeControllerProps)=>
{

    const [officeState, setOfficeState] = React.useState(true);
    const _toggleOfficeStates = (bool) => {
      setOfficeState(bool);
      };
    const dividerClass= divider();
    const centerClass = center();
    const list_holderClass = list_holder();


    return (<div><div className={centerClass.root}>
      <ButtonDiscrete onClick={_toggleOfficeStates(true)}>Change Office</ButtonDiscrete>
      <div className={dividerClass.root}/>
      <ButtonDiscreteWithEndIcon onClick={_toggleOfficeStates(false)}> Who Booked</ButtonDiscreteWithEndIcon>
      {
        officeStateSelector(officeState)
      }
      </div> </div>)
}
