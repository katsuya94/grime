package cmd

import (
	"github.com/katsuya94/grime/gen"
	"github.com/spf13/cobra"
)

func init() {
	rootCmd.AddCommand(genCmd)
}

var genCmd = &cobra.Command{
	Use:   "gen",
	Short: "Generate Grime bindings for a given Go package",
	RunE: func(cmd *cobra.Command, args []string) error {
		for _, arg := range args {
			err := gen.Generate(arg)
			if err != nil {
				return err
			}
		}
		return nil
	},
}
