package cmd

import (
	"os"

	"github.com/katsuya94/grime/read"
	"github.com/spf13/cobra"
)

func init() {
	rootCmd.AddCommand(runCmd)
}

var runCmd = &cobra.Command{
	Use:   "run",
	Short: "Run a Grime top-level program",
	Args:  cobra.ExactArgs(1),
	RunE: func(cmd *cobra.Command, args []string) error {
		rt, err := newRuntime()
		if err != nil {
			return err
		}
		f, err := os.Open(args[0])
		if err != nil {
			return err
		}
		topLevelProgram, err := read.Read(f)
		if err != nil {
			return err
		}
		return rt.Execute(topLevelProgram)
	},
}
