defmodule CsvSample.Order do
  defstruct [
    :gender,
    :age,
    :oil_level,
    :hard_level,
    :salt_level,
    :topping,
    :rice,
    :review
  ]

  def new(%{
        "性別" => gender,
        "年齢層" => age,
        "脂" => oil_level,
        "濃さ" => salt_level,
        "硬さ" => hard_level,
        "トッピング" => topping,
        "ライス" => rice,
        "評価" => review
      }) do
    %__MODULE__{
      topping: topping,
      rice: rice,
      age: age,
      gender: gender,
      oil_level: oil_level,
      salt_level: salt_level,
      hard_level: hard_level,
      review: review |> String.to_integer()
    }
  end

  def new(_), do: {:error, :required_key_does_not_exit}
end
