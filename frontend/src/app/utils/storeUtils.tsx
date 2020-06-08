import { Dispatch } from "@reduxjs/toolkit";
import { IRootStore } from "app/stores/rootStore";
import { IServices } from "app/services/services";

export type AsyncAction<T = unknown> = (
  dispatch: Dispatch,
  getState: () => IRootStore,
  services: IServices
) => Promise<T>;
