package cmd

import (
	"io"
	"os"

	"github.com/katsuya94/grime/common"
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
		rt := newRuntime()
		f, err := os.Open(args[0])
		if err != nil {
			return err
		}
		reader := read.NewDatumReader(f)
		var topLevelProgram []common.WrappedSyntax
		for {
			if datum, err := reader.ReadDatum(); err == io.EOF {
				break
			} else if err != nil {
				return err
			} else {
				topLevelProgram = append(topLevelProgram, common.NewWrappedSyntax(datum))
			}
		}
		return rt.Execute(topLevelProgram)
	},
}
