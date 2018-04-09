defmodule Cclock do
    use GenServer  
    require Logger


    def start_link(state) do
        import Supervisor.Spec
        Logger.info("#{__MODULE__}.start_link(#{inspect state})...")
        Logger.info("#{__MODULE__} starting [:cclock_sup, Columbo")
        children = [
          Columbo,
          supervisor(:cclock_sup, []),
        ]
        {:ok, super_pid} = Supervisor.start_link(children, [strategy: :one_for_one, name: Cclock.Supervisor])
        Logger.info("#{__MODULE__} started children: #{inspect super_pid}")
        {:ok, super_pid}
    end

    def init(state) do
        {:ok, state}
    end
  end