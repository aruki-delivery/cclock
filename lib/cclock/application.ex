defmodule Cclock.Application do
    use Application
    require Logger

    def start(type, args) do
      Logger.info("#{__MODULE__}.start(#{inspect type}}, #{inspect args})...")
      {:ok, cclock_sup} = Supervisor.start_link([Cclock], [strategy: :one_for_one, name: Cclock.Application.Supervisor])
      Logger.info("#{__MODULE__} started Cclock - #{inspect cclock_sup}")
      {:ok, cclock_sup}
    end
end