import React, { FormEvent } from "react";
import { useDispatch } from "react-redux";
import { userStore, IUser } from "app/stores/userStore";
import { useServices } from "app/services/services";
import { RoutePaths } from "app/ui/app/AppRoutes";

export const FakeLoginPage: React.FC = () => {
  const { historyService } = useServices();
  const dispatch = useDispatch();

  const onLoginSubmit = (ev: FormEvent<HTMLFormElement>) => {
    ev.preventDefault();

    const formData = new FormData(ev.currentTarget);
    const user = (Object.fromEntries(formData.entries()) as any) as IUser;

    if (typeof user.email === "string") {
      dispatch(userStore.actions.setUser(user));
      historyService.push(RoutePaths.Entrance);
    }
  };

  return (
    <div className="FakeLoginPage">
      <form onSubmit={onLoginSubmit}>
        <label>
          <span>Email: </span>
          <input type="email" name="email" />
        </label>

        <label>
          <span>Site: </span>
          <select name="site">
            <option value="Berlin">Berlin</option>
            <option value="Munich">Munich</option>
          </select>
        </label>

        <button type="submit">Login</button>
      </form>
    </div>
  );
};
