function Branch({ trunk }: { trunk: boolean }) {
  return (
    <div
      className={`
            ${!trunk ? `border-l-4` : ""} 
            ${trunk ? "w-24" : "w-16"}
            ${trunk ? "-translate-x-24" : "-translate-x-16"} 
            scale-y-110
            transition
            ease-in-out
            duration-300
            group-hover:border-teal-400
            h-full border-blue-500 border-b-4 absolute -translate-y-10`}
    />
  );
}

export default Branch;
